-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Calendar;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Numerics.MT19937;
with Ada.Streams.Stream_IO.Standards;
with Ada.Strings.Unbounded;
with Web.Lock_Files;
with Tabula.Calendar;
with Tabula.Debug;
with Tabula.Users.Lists;
with Tabula.Casts.Load;
with Tabula.Villages.Lists;
with Vampire.Configurations;
with Vampire.Forms.Select_Form;
with Vampire.Log;
with Vampire.R3.Index_Page;
with Vampire.R3.Message_Page;
with Vampire.R3.Preview_Page;
with Vampire.R3.Refresh_Page;
with Vampire.R3.Register_Page;
with Vampire.R3.Target_Page;
with Vampire.R3.User_List_Page;
with Vampire.R3.User_Page;
with Vampire.R3.Village_Page;
with Vampire.Villages.Advance;
with Vampire.Villages.Load;
with Vampire.Villages.Save;
procedure Vampire.Main is
	use type Ada.Calendar.Time;
	use type Ada.Strings.Unbounded.Unbounded_String;
	use type Casts.Person_Sex;
	use type Casts.Work;
	use type Users.Lists.User_State;
	use type Tabula.Villages.Village_State;
	use type Tabula.Villages.Village_Term;
	use type Forms.Base_Page;
	use type Villages.Ability_State;
	use type Villages.Attack_Mode;
	use type Villages.Doctor_Infected_Mode;
	use type Villages.Daytime_Preview_Mode;
	use type Villages.Person_Role;
	use type Villages.Person_State;
	use type Villages.Message_Kind;
	use type Villages.Message;
	use type Villages.Village_Time;
	use type Villages.Vote_State_Type;
	
	-- 現在時刻
	Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
	
	-- 標準入出力
	Input : not null Ada.Streams.Stream_IO.Stream_Access :=
		Ada.Streams.Stream_IO.Stream (Ada.Streams.Stream_IO.Standards.Standard_Input.all);
	Output : not null Ada.Streams.Stream_IO.Stream_Access :=
		Ada.Streams.Stream_IO.Stream (Ada.Streams.Stream_IO.Standards.Standard_Output.all);
	
	-- 乱数シード
	Generator : aliased Ada.Numerics.MT19937.Generator := Ada.Numerics.MT19937.Initialize;
	
	-- ユーザー情報
	User_List : Users.Lists.User_List := Users.Lists.Create (
		Directory => Configurations.Users_Directory'Access,
		Log_File_Name => Configurations.Users_Log_File_Name'Access);
	
	-- 村情報
	Village_List : Tabula.Villages.Lists.Village_List := Tabula.Villages.Lists.Create (
		Data_Directory => Configurations.Villages_Data_Directory'Access,
		HTML_Directory => Configurations.Villages_HTML_Directory'Access,
		Blocking_Short_Term_File_Name => Configurations.Villages_Blocking_Short_Term_File_Name'Access,
		Cache_File_Name => Configurations.Villages_Cache_File_Name'Access,
		Create_Index => Log.Create_Index'Access,
		Types => (
			1 => (
				Type_Code => Log.Type_Code'Access,
				Load_Summary => Log.Load_Summary'Access,
				Create_Log => Log.Create_Log'Access)));
	
begin
	if not Ada.Environment_Variables.Exists ("USE_STDERR") then
		Debug.Hook (Configurations.Debug_Log_File_Name'Access, Now);
	end if;
	Ada.Environment_Variables.Set ("TMPDIR", Configurations.Temporary_Directory);
	Locked : declare
		Lock : Web.Lock_Files.Lock_Type := Web.Lock_Files.Lock (Configurations.Lock_Name, Force => 60.0);
		-- HTTP Info
		Remote_Addr : String renames Web.Remote_Addr;
		Remote_Host : String renames Web.Remote_Host;
		Inputs : Web.Query_Strings renames Web.Get (Input);
		Query_Strings : Web.Query_Strings renames Web.Get_Query_Strings;
		Cookie : Web.Cookie := Web.Get_Cookie; -- variable
		Post : Boolean renames Web.Post;
		pragma Warnings (Off); -- compiler...
		Form : Forms.Root_Form_Type'Class := Forms.Select_Form (
		   Query_Strings,
		   Speeches_Per_Page => Configurations.Speeches_Per_Page);
		pragma Warnings (On);
		procedure Refresh_Page is
			Self_URI : constant String := Web.Request_URI;
		begin
			Web.Header_See_Other (Output, Self_URI);
			Web.Header_Content_Type (Output, Web.Text_HTML);
			Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
			Web.Header_Break (Output);
			R3.Refresh_Page (Output, Form, URI => Self_URI);
		end Refresh_Page;
		-- Values
		User_Id : constant String := Form.Get_User_Id (Query_Strings, Cookie);
		User_Password : constant String := Form.Get_User_Password (Query_Strings, Cookie);
		Base_Page : constant Forms.Base_Page := Form.Get_Base_Page (Query_Strings, Cookie);
		Village_Id : constant Tabula.Villages.Village_Id := Form.Get_Village_Id (Query_Strings);
		Cmd : constant String := Form.Get_Command (Inputs);
	begin
		if Web.Lock_Files.Forced (Lock) then
			Ada.Debug.Put ("forced to remove lock-file.");
		end if;
		if Post and then (Remote_Host = "" or else Remote_Host = Remote_Addr)
			and then Remote_Addr /= "127.0.0.1" -- localhost
			and then Remote_Addr /= "::1" -- IPv6 localhost
			and then Remote_Addr /= "202.95.187.49" -- CATV
		then
			Web.Header_503 (Output);
			Web.Header_Break (Output);
		elsif Cmd = "logon" then
			Logon : declare
				New_User_Id : constant String := Form.Get_New_User_Id (Inputs);
				New_User_Password : constant String := Form.Get_New_User_Password (Inputs);
				User_State : Users.Lists.User_State;
				User_Info : Users.User_Info;
			begin
				Users.Lists.Query (User_List,
					Id => New_User_Id, Password => New_User_Password,
					Remote_Addr => Remote_Addr, Remote_Host => Remote_Host,
					Now => Now,
					Info => User_Info, State => User_State);
				case User_State is
					when Users.Lists.Log_Off =>
						Web.Header_Content_Type (Output, Web.Text_HTML);
						Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
						Web.Header_Break (Output);
						R3.Message_Page (
							Output,
							Form,
							Configurations.Template_Names (Form.Template_Set).Template_Message_File_Name.all,
							Base_Page => Base_Page,
							Village_Id => Village_Id,
							Message => "ユーザー名を入力してください。",
							User_Id => "",
							User_Password => "");
					when Users.Lists.Unknown =>
						if Users.Valid_Id_String(New_User_Id) then
							Web.Header_Content_Type (Output, Web.Text_HTML);
							Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
							Web.Header_Break (Output);
							R3.Register_Page(
								Output,
								Form,
								Configurations.Template_Names (Form.Template_Set).Template_Register_File_Name.all,
								Base_Page => Base_Page,
								Village_Id => Village_Id,
								New_User_Id => New_User_Id,
								New_User_Password => New_User_Password);
						else
							Web.Header_Content_Type (Output, Web.Text_HTML);
							Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
							Web.Header_Break (Output);
							R3.Message_Page (
								Output,
								Form,
								Configurations.Template_Names (Form.Template_Set).Template_Message_File_Name.all,
								Base_Page => Base_Page,
								Village_Id => Village_Id,
								Message => "申し訳ありませんがユーザー名に使えない文字が含まれています。",
								User_Id => "",
								User_Password => "");
						end if;
					when Users.Lists.Invalid =>
						Web.Header_Content_Type (Output, Web.Text_HTML);
						Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
						Web.Header_Break (Output);
						R3.Message_Page (
							Output,
							Form,
							Configurations.Template_Names (Form.Template_Set).Template_Message_File_Name.all,
							Base_Page => Base_Page,
							Village_Id => Village_Id,
							Message => "パスワードが異なります。",
							User_Id => "",
							User_Password => "");
					when Users.Lists.Valid =>
						Forms.Set_User (
							Form,
							Cookie,
							New_User_Id => New_User_Id,
							New_User_Password => New_User_Password);
						Web.Header_Content_Type (Output, Web.Text_HTML);
						Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
						Web.Header_Break (Output);
						R3.Message_Page (
							Output,
							Form,
							Configurations.Template_Names (Form.Template_Set).Template_Message_File_Name.all,
							Base_Page => Base_Page,
							Village_Id => Village_Id,
							Message => "ログオンしました。",
							User_Id => New_User_Id,
							User_Password => New_User_Password);
						Users.Lists.Update (User_List,
							Id => New_User_Id,
							Remote_Addr => Remote_Addr,
							Remote_Host => Remote_Host,
							Now => Now,
							Info => User_Info);
				end case;
			end Logon;
		elsif Cmd = "logoff" then
			Logoff : declare
				User_State : Users.Lists.User_State;
				User_Info : Users.User_Info;
				Dest_Page : Forms.Base_Page;
			begin
				Users.Lists.Query (User_List,
					Id => User_Id, Password => User_Password,
					Remote_Addr => Remote_Addr, Remote_Host => Remote_Host,
					Now => Now,
					Info => User_Info, State => User_State);
				case User_State is
					when Users.Lists.Valid =>
						Users.Lists.Update (User_List,
							Id => User_Id,
							Remote_Addr => Remote_Addr,
							Remote_Host => Remote_Host,
							Now => Now,
							Info => User_Info);
					when others =>
						null;
				end case;
				Forms.Set_User (Form, Cookie, New_User_Id => "", New_User_Password => ""); -- logoff
				if Base_Page = Forms.User_Page then
					Dest_Page := Forms.Index_Page;
				else
					Dest_Page := Base_Page;
				end if;
				Web.Header_Content_Type (Output, Web.Text_HTML);
				Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
				Web.Header_Break (Output);
				R3.Message_Page (
					Output,
					Form,
					Configurations.Template_Names (Form.Template_Set).Template_Message_File_Name.all,
					Base_Page => Dest_Page,
					Village_Id => Village_Id,
					Message => "ログオフしました。",
					User_Id => "",
					User_Password => "");
			end Logoff;
		elsif Cmd = "register" then
			Register : declare
				New_User_Id : constant String := Form.Get_New_User_Id (Inputs);
				New_User_Password : constant String := Form.Get_New_User_Password (Inputs);
				New_User_Password_Retype : constant String := Form.Get_New_User_Confirmation_Password (Inputs);
				Registered : Boolean;
			begin
				if New_User_Password /= New_User_Password_Retype then
					Web.Header_Content_Type (Output, Web.Text_HTML);
					Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
					Web.Header_Break (Output);
					R3.Message_Page (
						Output,
						Form,
						Configurations.Template_Names (Form.Template_Set).Template_Message_File_Name.all,
						Base_Page => Base_Page,
						Village_Id => Village_Id,
						Message => "再入力されたパスワードが異なります。",
						User_Id => "",
						User_Password => "");
				else
					Users.Lists.New_User (User_List,
						Id => New_User_Id, Password => New_User_Password,
						Remote_Addr => Remote_Addr, Remote_Host => Remote_Host,
						Now => Now,
						Result => Registered);
					if Registered then
						Forms.Set_User (
							Form,
							Cookie,
							New_User_Id => New_User_Id,
							New_User_Password => New_User_Password);
						Web.Header_Content_Type (Output, Web.Text_HTML);
						Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
						Web.Header_Break (Output);
						R3.Message_Page (
							Output,
							Form,
							Configurations.Template_Names (Form.Template_Set).Template_Message_File_Name.all,
							Base_Page => Base_Page,
							Village_Id => Village_Id,
							Message => "登録に成功しました。",
							User_Id => New_User_Id,
							User_Password => New_User_Password);
					else
						Web.Header_Content_Type (Output, Web.Text_HTML);
						Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
						Web.Header_Break (Output);
						R3.Message_Page (
							Output,
							Form,
							Configurations.Template_Names (Form.Template_Set).Template_Message_File_Name.all,
							Base_Page => Base_Page,
							Village_Id => Village_Id,
							Message => "登録に失敗しました。",
							User_Id => "",
							User_Password => "");
					end if;
				end if;
			end Register;
		else
			Valid_User : declare
				User_State : Users.Lists.User_State;
				User_Info : Users.User_Info;
			begin
				Users.Lists.Query (User_List,
					Id => User_Id, Password => User_Password,
					Remote_Addr => Remote_Addr, Remote_Host => Remote_Host,
					Now => Now,
					Info => User_Info, State => User_State);
				if User_State = Users.Lists.Invalid then
					Forms.Set_User (Form, Cookie, New_User_Id => "", New_User_Password => ""); -- logoff
					Web.Header_Content_Type (Output, Web.Text_HTML);
					Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
					Web.Header_Break (Output);
					R3.Message_Page (
						Output,
						Form,
						Configurations.Template_Names (Form.Template_Set).Template_Message_File_Name.all,
						Base_Page => Base_Page,
						Village_Id => Village_Id,
						Message => "おや？ なにかおかしいです。 ログインし直してください。",
						User_Id => "",
						User_Password => "");
				elsif Cmd = "newl" or else Cmd = "news" then -- 村作成
					New_Village : declare
						Term : Tabula.Villages.Village_Term;
						Summaries : Tabula.Villages.Lists.Summary_Maps.Map;
					begin
						if Cmd = "news" then
							Term := Tabula.Villages.Short;
						else
							Term := Tabula.Villages.Long;
						end if;
						case User_State is
							when Users.Lists.Valid =>
								Tabula.Villages.Lists.Get_Summaries (Village_List, Summaries);
								if User_Id /= Users.Administrator
									and then Tabula.Villages.Lists.Exists_Opened_By (Summaries, User_Id)
								then
									Web.Header_Content_Type (Output, Web.Text_HTML);
									Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
									Web.Header_Break (Output);
									R3.Message_Page (
										Output,
										Form,
										Configurations.Template_Names (Form.Template_Set).Template_Message_File_Name.all,
										Base_Page => Base_Page,
										Message => "同時に村をふたつ作成することはできません。",
										User_Id => User_Id,
										User_Password => User_Password);
								elsif Term = Tabula.Villages.Short
									and then (User_Info.Disallow_New_Village
										or else (
											Tabula.Villages.Lists.Blocking_Short_Term (Village_List)
											and then User_Id /= Users.Administrator
											and then User_Id /= "she")) -- ハードコーディングですよ酷いコードですね
								then
									Web.Header_Content_Type (Output, Web.Text_HTML);
									Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
									Web.Header_Break (Output);
									R3.Message_Page (
										Output,
										Form,
										Configurations.Template_Names (Form.Template_Set).Template_Message_File_Name.all,
										Base_Page => Base_Page,
										Message => "えーと、しばらく短期は延期で。",
										User_Id => User_Id,
										User_Password => User_Password);
								else
									declare
										Village_Name : constant String := Form.Get_New_Village_Name (Inputs);
									begin
										if Village_Name = "" then
											Web.Header_Content_Type (Output, Web.Text_HTML);
											Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
											Web.Header_Break (Output);
											R3.Message_Page (
												Output,
												Form,
												Configurations.Template_Names (Form.Template_Set).Template_Message_File_Name.all,
												Base_Page => Base_Page,
												Message => "村名を入力してください。",
												User_Id => User_Id,
												User_Password => User_Password);
										else
											declare
												New_Village : Villages.Village_Type := Villages.Create (
													Name => Village_Name,
													By => User_Id,
													Term => Term,
													Time => Now);
												New_Village_Id : constant Tabula.Villages.Village_Id :=
													Tabula.Villages.Lists.New_Village_Id (Village_List);
											begin
												Villages.Save (
													Tabula.Villages.Lists.File_Name (Village_List, New_Village_Id),
													New_Village);
												Tabula.Villages.Lists.Update (
													Village_List,
													New_Village_Id,
													Tabula.Villages.Lists.Summary (
														Log.Type_Code,
														New_Village));
												Users.Lists.Update (User_List,
													Id => User_Id,
													Remote_Addr => Remote_Addr,
													Remote_Host => Remote_Host,
													Now => Now,
													Info => User_Info);
												Web.Header_Content_Type (Output, Web.Text_HTML);
												Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
												Web.Header_Break (Output);
												R3.Message_Page (
													Output,
													Form,
													Configurations.Template_Names (Form.Template_Set).Template_Message_File_Name.all,
													Base_Page => Base_Page,
													Message => "新たな村「" & Village_Name & "」を作成しました。",
													User_Id => User_Id,
													User_Password => User_Password);
											exception
												when Ada.IO_Exceptions.Name_Error =>
													Web.Header_Content_Type (Output, Web.Text_HTML);
													Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
													Web.Header_Break (Output);
													R3.Message_Page (
														Output,
														Form,
														Configurations.Template_Names (Form.Template_Set).Template_Message_File_Name.all,
														Base_Page => Base_Page,
														Message => "作成に失敗しました。",
														User_Id => User_Id,
														User_Password => User_Password);
											end;
										end if;
									end;
								end if;
							when others =>
								Web.Header_Content_Type (Output, Web.Text_HTML);
								Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
								Web.Header_Break (Output);
								R3.Message_Page (
									Output,
									Form,
									Configurations.Template_Names (Form.Template_Set).Template_Message_File_Name.all,
									Base_Page => Base_Page,
									Message => "ログオンしていないと村は作成できません。",
									User_Id => "",
									User_Password => "");
						end case;
					end New_Village;
				elsif Cmd = "remakelog" then
					if User_State = Users.Lists.Valid and then User_Id = Tabula.Users.Administrator then
						Tabula.Villages.Lists.Refresh (Village_List);
						Refresh_Page;
					else
						Web.Header_Content_Type (Output, Web.Text_HTML);
						Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
						Web.Header_Break (Output);
						R3.Message_Page (
							Output,
							Form,
							Configurations.Template_Names (Form.Template_Set).Template_Message_File_Name.all,
							Base_Page => Forms.Index_Page,
							Message => "administratorのみに許された操作です。",
							User_Id => "",
							User_Password => "");
					end if;
				elsif Base_Page /= Forms.Village_Page then
					-- index page, user page, all users page
					if Cmd = "" then
						if Post then
							Refresh_Page;
						elsif Base_Page = Forms.User_Page then
							if User_State = Users.Lists.Valid and then User_Id /= Users.Administrator then
								declare
									Summaries : Tabula.Villages.Lists.Summary_Maps.Map;
								begin
									Tabula.Villages.Lists.Get_Summaries (Village_List, Summaries);
									Web.Header_Content_Type (Output, Web.Text_HTML);
									Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
									Web.Header_Break (Output);
									R3.User_Page (
										Output,
										Form,
										Configurations.Template_Names (Form.Template_Set).Template_User_File_Name.all,
										Summaries,
										User_Id => User_Id,
										User_Password => User_Password,
										User_Info => User_Info);
								end;
							else
								Web.Header_Content_Type (Output, Web.Text_HTML);
								Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
								Web.Header_Break (Output);
								R3.Message_Page (
									Output,
									Form,
									Configurations.Template_Names (Form.Template_Set).Template_Message_File_Name.all,
									Base_Page => Forms.Index_Page,
									Message => "ログオンしないとユーザーページは表示できません。",
									User_Id => "",
									User_Password => "");
							end if;
						elsif Base_Page = Forms.User_List_Page then
							declare
								Summaries : Tabula.Villages.Lists.Summary_Maps.Map;
							begin
								Tabula.Villages.Lists.Get_Summaries (Village_List, Summaries);
								Web.Header_Content_Type (Output, Web.Text_HTML);
								Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
								Web.Header_Break (Output);
								R3.User_List_Page (
									Output,
									Form,
									Configurations.Template_Names (Form.Template_Set).Template_User_List_File_Name.all,
									HTML_Directory => Configurations.Villages_HTML_Directory,
									Summaries => Summaries,
									User_List => Users.Lists.All_Users (User_List),
									User_Id => User_Id,
									User_Password => User_Password);
							end;
						else -- index page
							declare
								Summaries : Tabula.Villages.Lists.Summary_Maps.Map;
								Muramura_Count : Natural;
							begin
								Tabula.Villages.Lists.Get_Summaries (Village_List, Summaries);
								Users.Lists.Muramura_Count (User_List, Now, Configurations.Muramura_Duration, Muramura_Count);
								Web.Header_Content_Type (Output, Web.Text_HTML);
								Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
								Web.Header_Break (Output);
								R3.Index_Page (
									Output,
									Form,
									Configurations.Template_Names (Form.Template_Set).Template_Index_File_Name.all,
									HTML_Directory => Configurations.Villages_HTML_Directory,
									Summaries => Summaries,
									Muramura => Muramura_Count,
									User_Id => User_Id,
									User_Password => User_Password);
							end;
						end if;
					else
						Web.Header_Content_Type (Output, Web.Text_HTML);
						Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
						Web.Header_Break (Output);
						R3.Message_Page (
							Output,
							Form,
							Configurations.Template_Names (Form.Template_Set).Template_Message_File_Name.all,
							Base_Page => Base_Page,
							Message => "不正なコマンド(" & Cmd & ")が送られました。",
							User_Id => User_Id,
							User_Password => User_Password);
					end if;
				elsif not Tabula.Villages.Lists.Exists (Village_List, Village_Id) then
					Web.Header_Content_Type (Output, Web.Text_HTML);
					Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
					Web.Header_Break (Output);
					R3.Message_Page (
						Output,
						Form,
						Configurations.Template_Names (Form.Template_Set).Template_Message_File_Name.all,
						Base_Page => Forms.Index_Page,
						Message => "存在しない村が指定されました。",
						User_Id => User_Id,
						User_Password => User_Password);
				else
					Village_Page : declare
						Village : aliased Villages.Village_Type;
						procedure Message_Page (Message : in String) is
						begin
							Web.Header_Content_Type (Output, Web.Text_HTML);
							Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
							Web.Header_Break (Output);
							R3.Message_Page (
								Output,
								Form,
								Configurations.Template_Names (Form.Template_Set).Template_Message_File_Name.all,
								Base_Page => Forms.Village_Page,
								Village_Id => Village_Id,
								Village => Village'Access,
								Message => Message,
								User_Id => User_Id,
								User_Password => User_Password);
						end Message_Page;
					begin
						Villages.Load (Tabula.Villages.Lists.File_Name (Village_List, Village_Id), Village);
						-- Village.Name := +Village.Name.Constant_Reference.Element.all; -- dirty hack for memory bug
						if Cmd = "" then
							if Post then
								Refresh_Page;
							else
								-- 強制進行
								declare
									Changed, List_Changed : Boolean;
								begin
									Villages.Advance(Village, Now, Generator'Access,
										Changed => Changed, List_Changed => List_Changed);
									if Changed then
										Villages.Save (Tabula.Villages.Lists.File_Name (Village_List, Village_Id), Village);
									end if;
									if List_Changed then
										Tabula.Villages.Lists.Update (
											Village_List,
											Village_Id,
											Tabula.Villages.Lists.Summary (
												Log.Type_Code,
												Village));
									end if;
								end;
								-- 村レンダリング
								declare
									Day : Natural := Form.Get_Day (Village, Query_Strings);
									Message_Range : Tabula.Villages.Message_Range_Type := Form.Get_Range (Village, Day, Query_Strings);
								begin
									Web.Header_Content_Type (Output, Web.Text_HTML);
									Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
									Web.Header_Break (Output);
									R3.Village_Page (
										Output,
										Form,
										R3.Read (Configurations.Template_Names (Form.Template_Set).Template_Village_File_Name.all),
										Current_Directory => ".",
										HTML_Directory => Configurations.Villages_HTML_Directory,
										Image_Directory => Configurations.Template_Names (Form.Template_Set).Image_Directory.all,
										Style_Sheet => Configurations.Template_Names (Form.Template_Set).Style_Sheet_File_Name.all,
										Background => Configurations.Template_Names (Form.Template_Set).Background_Image_File_Name.all,
										Relative_Role_Images => Configurations.Template_Names (Form.Template_Set).Relative_Role_Image_File_Names.all,
										Cast_File_Name => Configurations.Cast_File_Name,
										Log => False,
										Village_Id => Village_Id,
										Village => Village,
										Day => Day,
										Showing_Range => Message_Range,
										User_Id => User_Id,
										User_Password => User_Password);
								end;
							end if;
						elsif Village.State = Tabula.Villages.Closed then
							Message_Page ("終了した村にコマンドが送られました。");
						elsif User_State /= Users.Lists.Valid then
							Message_Page ("正常にログオンしてください。");
						else
							Commands : declare
								Player : Integer := Villages.Joined(Village, User_Id);
							begin
								if Cmd = "join" then
									Join : declare
										Summaries : Tabula.Villages.Lists.Summary_Maps.Map;
									begin
										Tabula.Villages.Lists.Get_Summaries (Village_List, Summaries);
										if Player /= Tabula.Villages.No_Person then
											Message_Page ("既にこの村に参加しています。");
										elsif Village.State /= Tabula.Villages.Prologue then
											Message_Page ("村が開始されたため今からは参加できません。 次の村をよろしくお願いします。");
										elsif Village.People.Length >= Villages.Maximum_Number_Of_Persons then
											Message_Page ("定員に達したため今からは参加できません。 次の村をよろしくお願いします。");
										elsif Tabula.Villages.Lists.Exists_Opened_By (Summaries, User_Id, Excluding => Village_Id) then
											Message_Page ("自分の作成した村に入ってください。");
										elsif Village.Term = Tabula.Villages.Long
											and then Tabula.Villages.Lists.Count_Joined_By (
												Summaries,
												User_Id,
												Filter => (
													Tabula.Villages.Prologue | Tabula.Villages.Playing => True,
													Tabula.Villages.Epilogue | Tabula.Villages.Closed => False),
												Long_Only => True) > 0
										then
											Message_Page ("既に他の村に参加しています。");
										else
											declare
												Joining : Forms.Joining := Form.Get_Joining (Inputs);
												Cast : Casts.Cast_Collection := Casts.Load (Configurations.Cast_File_Name);
											begin
												Villages.Exclude_Taken (Cast, Village);
												declare
													Person_Template : Casts.Person
														renames Cast.People.Constant_Reference (Joining.Name_Index).Element.all;
												begin
													if Joining.Work_Index < 0 then
														Joining.Work_Index := Casts.Find (
															Cast.Works,
															Person_Template.Work.Constant_Reference.Element.all);
													end if;
													if Joining.Work_Index < 0 then
														Message_Page (
															"申し訳ありませんが既定の肩書き(" &
															Person_Template.Work.Constant_Reference.Element.all &
															")は既に他の方に取られています。");
													else
														declare
															Selected_Work : Casts.Work
																renames Cast.Works.Constant_Reference (Joining.Work_Index).Element.all;
														begin
															if Person_Template.Name = "" or Selected_Work.Name = "" then
																Message_Page ("選択した顔または肩書きは既に他の方に取られています。");
															elsif Selected_Work.Sex /= Casts.Neutral and then Selected_Work.Sex /= Person_Template.Sex then
																Message_Page ("性別と肩書きが一致しません。");
															elsif Selected_Work.Nominated and then Selected_Work.Name /= Person_Template.Work then
																Message_Page ("その肩書きは特定の組み合わせでしか使えません。");
															elsif Villages.Already_Joined_As_Another_Sex(Village, User_Id, Person_Template.Sex) then
																Message_Page ("以前にエントリされたときと性別が異なります。");
															else
																Villages.Join (
																	Village,
																	User_Id,
																	Person_Template,
																	Selected_Work,
																	Villages.Requested_Role'Value (Joining.Request.Constant_Reference.Element.all),
																	User_Info.Ignore_Request,
																	Now);
																Villages.Save (Tabula.Villages.Lists.File_Name (Village_List, Village_Id), Village);
																Tabula.Villages.Lists.Update (
																	Village_List,
																	Village_Id,
																	Tabula.Villages.Lists.Summary (
																		Log.Type_Code,
																		Village));
																Message_Page ("村に参加しました。");
															end if;
														end;
													end if;
												end;
											end;
										end if;
									end Join;
								elsif Cmd = "narration" then
									if User_Id /= Tabula.Users.Administrator then
										Message_Page ("administratorのみに許された操作です。");
									else
										declare
											Text : constant String := Form.Get_Text (Inputs);
										begin
											Villages.Narration (Village, Text, Now);
											Villages.Save (
												Tabula.Villages.Lists.File_Name (Village_List, Village_Id),
												Village);
										end;
										Refresh_Page;
									end if;
								elsif Cmd = "remove" then
									if Village.State /= Tabula.Villages.Prologue then
										Message_Page ("除名を行えるのはプロローグのみです。");
									elsif User_Id /= Tabula.Users.Administrator then
										Message_Page ("administratorのみに許された操作です。");
									else
										declare
											Target : constant Integer := Form.Get_Target (Inputs);
											Removed_Id : constant String :=
												Village.People.Constant_Reference (Target).Element.
													Id.Constant_Reference.Element.all;
										begin
											Villages.Escape(Village, Target, Now);
											Villages.Save (
												Tabula.Villages.Lists.File_Name (Village_List, Village_Id),
												Village);
											Tabula.Villages.Lists.Update (
												Village_List,
												Village_Id,
												Tabula.Villages.Lists.Summary (
													Log.Type_Code,
													Village));
											Message_Page (Removed_Id & "を村から除名しました。");
										end;
									end if;
								elsif Player < 0 then
									Message_Page ("村に参加していない状態でコマンド(" & Cmd & ")が送られました。");
								elsif Cmd = "commit" then
									if Village.Time = Villages.Night then
										Message_Page ("今は行動を終えられない時間帯です。");
									elsif Village.People.Constant_Reference(Player).Element.
										Records.Constant_Reference(Village.Today).Element.State = Villages.Died
									then
										Message_Page ("死者は行動を決定できません。");
									else
										if not Village.People.Constant_Reference(Player).Element.Commited then
											Village.People.Reference(Player).Element.Commited := True;
											declare
												Changed, List_Changed : Boolean;
											begin
												Villages.Advance(Village, Now, Generator'Access,
													Changed => Changed, List_Changed => List_Changed);
												Villages.Save (Tabula.Villages.Lists.File_Name (Village_List, Village_Id), Village);
												if List_Changed then
													Tabula.Villages.Lists.Update (
														Village_List,
														Village_Id,
														Tabula.Villages.Lists.Summary (
															Log.Type_Code,
															Village));
												end if;
											end;
										end if;
										Refresh_Page;
									end if;
								elsif Cmd = "rollback" then
									if Village.People.Constant_Reference(Player).Element.Commited then
										Village.People.Reference(Player).Element.Commited := False;
										Villages.Save (Tabula.Villages.Lists.File_Name (Village_List, Village_Id), Village);
									end if;
									Refresh_Page;
								elsif Cmd = "escape" then
									if Village.State /= Tabula.Villages.Prologue then
										Message_Page ("村から出られるのはプロローグのみです。");
									else
										case Form.Get_Answered (Inputs) is
											when Forms.Missing =>
												Message_Page ("答えを入力してください。");
											when Forms.NG =>
												Message_Page ("答えが違います。");
											when Forms.OK =>
												Villages.Escape(Village, Player, Now);
												Villages.Save (Tabula.Villages.Lists.File_Name (Village_List, Village_Id), Village);
												Tabula.Villages.Lists.Update (
													Village_List,
													Village_Id,
													Tabula.Villages.Lists.Summary (
														Log.Type_Code,
														Village));
												Message_Page ("旅に出ました。");
										end case;
									end if;
								elsif Cmd = "action" then
									declare
										Action : constant String := Form.Get_Action (Inputs);
										Target : constant Integer := Form.Get_Target (Inputs);
										Said : Villages.Message_Counts
											renames Villages.Count_Messages(Village, Village.Today);
									begin
										if Target < 0 or else Action = "" then
											Message_Page ("アクションの対象と行動を選んでください。");
										elsif Action = "wake" then
											if Said(Player).Wake > 0 then
												Message_Page ("人を起こせるのは一日一度です。");
											elsif not Village.People.Constant_Reference(Target).Element.Commited then
												Message_Page ("相手はまだ行動を終えていません。");
											else
												Villages.Wake (Village, Player, Target, Now);
												Villages.Save (Tabula.Villages.Lists.File_Name (Village_List, Village_Id), Village);
												Refresh_Page;
											end if;
										elsif Action = "encourage" then
											if Said(Player).Encourage > 0 then
												Message_Page ("話の続きを促せるのは一日一度です。");
											else
												Villages.Encourage (Village, Player, Target, Now);
												Villages.Save (Tabula.Villages.Lists.File_Name (Village_List, Village_Id), Village);
												Refresh_Page;
											end if;
										elsif Action = "vampire_gaze" then
											if Village.People.Constant_Reference(Player).Element.Role not in Villages.Vampire_Role then
												Message_Page ("見つめられるのは吸血鬼だけです。");
											elsif Village.People.Constant_Reference(Target).Element.Role in Villages.Vampire_Role then
												Message_Page ("見つめようとした相手は吸血鬼です。");
											elsif Said(Player).Vampire_Gaze > 0 then
												Message_Page ("見つめられるのは一日一度です。");
											elsif Village.Time = Villages.Night then
												Message_Page ("今は夜です。 アクションを消費せずに直接会話できます。");
											elsif not Village.Can_Gaze then
												Message_Page ("1日目の途中の襲撃は必ず感染になりますので襲撃先を揃える必要はありません、夜の襲撃用に取っておきましょう。");
											else
												Villages.Gaze (Village, Player, Target, Now);
												Villages.Save (Tabula.Villages.Lists.File_Name (Village_List, Village_Id), Village);
												Refresh_Page;
											end if;
										else
											Message_Page ("未対応アクション(" & Action & ")を行おうとしました。");
										end if;
									end;
								elsif Cmd = "speech" or else Cmd = "speech2" then
									declare
										function Speech_Check return Boolean is
										begin
											if Village.State = Tabula.Villages.Playing
												and then Village.People.Constant_Reference(Player).Element.
													Records.Constant_Reference(Village.Today).Element.State = Villages.Died
											then
												Message_Page ("あなたは死にました。");
												return False;
											elsif Village.Time /= Villages.Daytime then
												Message_Page ("今は喋れない時間帯です。");
												return False;
											else
												return True;
											end if;
										end Speech_Check;
									begin
										if Cmd = "speech" then
											if Speech_Check then
												declare
													Text : constant String := Form.Get_Text (Inputs);
												begin
													if Text'Length > 0 then
														Web.Header_Content_Type (Output, Web.Text_HTML);
														Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
														Web.Header_Break (Output);
														R3.Preview_Page (
															Output,
															Form,
															Configurations.Template_Names (Form.Template_Set).Template_Preview_File_Name.all,
															Image_Directory => Configurations.Template_Names (Form.Template_Set).Image_Directory.all,
															Village_Id => Village_Id,
															Village => Village,
															Message => Villages.Message'(
																Day => Village.Today,
																Time => Calendar.Null_Time,
																Kind => Villages.Speech,
																Subject => Player,
																Target => -1,
																Text => +Text),
															User_Id => User_Id,
															User_Password => User_Password);
													else
														Refresh_Page;
													end if;
												end;
											end if;
										else -- "speech2"
											if Speech_Check then
												declare
													Text : constant String := Form.Get_Text (Inputs);
												begin
													if Text'Length > 0 then
														Villages.Speech (Village, Player, Text, Now);
														Villages.Save (Tabula.Villages.Lists.File_Name (Village_List, Village_Id), Village);
													end if;
												end;
												Refresh_Page;
											end if;
										end if;
									end;
								elsif Cmd = "reedit" then
									Reedit : declare
										Reedit_Kind : constant Villages.Message_Kind :=
											Villages.Message_Kind'Value (Form.Get_Reedit_Kind (Inputs));
										Text : constant String := Form.Get_Text (Inputs);
										Day : Natural := Form.Get_Day (Village, Query_Strings);
										Message_Range : Tabula.Villages.Message_Range_Type := Form.Get_Range (Village, Day, Query_Strings);
									begin
										Web.Header_Content_Type (Output, Web.Text_HTML);
										Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
										Web.Header_Break (Output);
										R3.Village_Page (
											Output,
											Form,
											R3.Read (Configurations.Template_Names (Form.Template_Set).Template_Village_File_Name.all),
											Current_Directory => ".",
											HTML_Directory => Configurations.Villages_HTML_Directory,
											Image_Directory => Configurations.Template_Names (Form.Template_Set).Image_Directory.all,
											Style_Sheet => Configurations.Template_Names (Form.Template_Set).Style_Sheet_File_Name.all,
											Background => Configurations.Template_Names (Form.Template_Set).Background_Image_File_Name.all,
											Relative_Role_Images => Configurations.Template_Names (Form.Template_Set).Relative_Role_Image_File_Names.all,
											Cast_File_Name => Configurations.Cast_File_Name,
											Log => False,
											Village_Id => Village_Id,
											Village => Village,
											Day => Day,
											Showing_Range => Message_Range,
											Editing => Reedit_Kind,
											Editing_Text => Text,
											User_Id => User_Id,
											User_Password => User_Password);
									end Reedit;
								elsif Cmd = "monologue" then
									if Village.State /= Tabula.Villages.Playing then
										Message_Page ("プロローグ/エピローグでは独白は喋れません。");
									else
										declare
											Text : constant String := Form.Get_Text (Inputs);
										begin
											if Text'Length > Villages.Max_Length_Of_Message then
												Web.Header_Content_Type (Output, Web.Text_HTML);
												Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
												Web.Header_Break (Output);
												R3.Preview_Page(
													Output,
													Form,
													Configurations.Template_Names (Form.Template_Set).Template_Preview_File_Name.all,
													Image_Directory => Configurations.Template_Names (Form.Template_Set).Image_Directory.all,
													Village_Id => Village_Id,
													Village => Village,
													Message => Villages.Message'(
														Day => Village.Today,
														Time => Calendar.Null_Time,
														Kind => Villages.Monologue,
														Subject => Player,
														Target => -1,
														Text => +Text),
													User_Id => User_Id,
													User_Password => User_Password);
											else
												if Text /= "" then
													Villages.Monologue (Village, Player, Text, Now);
													Villages.Save (Tabula.Villages.Lists.File_Name (Village_List, Village_Id), Village);
												end if;
												Refresh_Page;
											end if;
										end;
									end if;
								elsif Cmd = "ghost" then
									if Village.State /= Tabula.Villages.Playing then
										Message_Page ("プロローグ/エピローグでは呻けません。");
									elsif Village.People.Constant_Reference(Player).Element.
										Records.Constant_Reference(Village.Today).Element.State /= Villages.Died
									then
										Message_Page ("生者は呻けません。");
									else
										declare
											Text : constant String := Form.Get_Text (Inputs);
										begin
											if Text'Length > Villages.Max_Length_Of_Message then
												Web.Header_Content_Type (Output, Web.Text_HTML);
												Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
												Web.Header_Break (Output);
												R3.Preview_Page (
													Output,
													Form,
													Configurations.Template_Names (Form.Template_Set).Template_Preview_File_Name.all,
													Image_Directory => Configurations.Template_Names (Form.Template_Set).Image_Directory.all,
													Village_Id => Village_Id,
													Village => Village,
													Message => Villages.Message'(
														Day => Village.Today,
														Time => Calendar.Null_Time,
														Kind => Villages.Ghost,
														Subject => Player,
														Target => -1,
														Text => +Text),
													User_Id => User_Id,
													User_Password => User_Password);
											else
												if Text /= "" then
													Villages.Ghost (Village, Player, Text, Now);
													Villages.Save (Tabula.Villages.Lists.File_Name (Village_List, Village_Id), Village);
												end if;
												Refresh_Page;
											end if;
										end;
									end if;
								elsif Cmd = "note" then
									declare
										Text : constant String := Form.Get_Text (Inputs);
									begin
										if Text'Length > Villages.Max_Length_Of_Message then
											Web.Header_Content_Type (Output, Web.Text_HTML);
											Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
											Web.Header_Break (Output);
											R3.Preview_Page (
												Output,
												Form,
												Configurations.Template_Names (Form.Template_Set).Template_Preview_File_Name.all,
												Image_Directory => Configurations.Template_Names (Form.Template_Set).Image_Directory.all,
												Village_Id => Village_Id,
												Village => Village,
												Message => Villages.Message'(
													Day => Village.Today,
													Time => Calendar.Null_Time,
													Kind => Villages.Howling,
													Subject => Player,
													Target => -1,
													Text => +Text),
												User_Id => User_Id,
												User_Password => User_Password);
										else
											if Text /= Village.People.Constant_Reference(Player).Element.Records.Constant_Reference(Village.Today).Element.Note then
												if Village.Time = Villages.Night
													and then Village.People.Constant_Reference(Player).Element.Role in Villages.Vampire_Role
													and then Village.People.Constant_Reference(Player).Element.Records.Constant_Reference(Village.Today).Element.State /= Villages.Died
												then
													Villages.Night_Talk (Village, Player, Text, Now);
												else
													Village.People.Reference(Player).Element.Records.Reference(Village.Today).Element.Note := +Text;
												end if;
												Villages.Save (Tabula.Villages.Lists.File_Name (Village_List, Village_Id), Village);
											end if;
											Refresh_Page;
										end if;
									end;
								elsif Cmd = "vote" then
									if Village.Vote_State = Villages.Disallowed then
										Message_Page ("今は投票できない時間帯です。");
									else
										declare
											Target : constant Integer := Form.Get_Target (Inputs);
										begin
											if Village.People.Constant_Reference(Player).Element.Records.Constant_Reference(Village.Today).Element.State = Villages.Died then
												Message_Page ("あなたは死んでいます。");
											elsif Target >= 0 and then Village.People.Constant_Reference(Target).Element.Records.Constant_Reference(Village.Today).Element.State = Villages.Died then
												Message_Page ("死者に投票はできません。");
											elsif Target >= 0 and then not Village.People.Constant_Reference(Target).Element.Records.Constant_Reference(Village.Today).Element.Candidate then
												Message_Page ("仮投票が行われました。 選ばれた候補以外に投票はできなくなります。");
											else
												if Target /= Village.People.Constant_Reference(Player).Element.Records.Constant_Reference(Village.Today).Element.Vote then
													Villages.Vote (Village, Player, Target);
													Villages.Save (Tabula.Villages.Lists.File_Name (Village_List, Village_Id), Village);
												end if;
												Refresh_Page;
											end if;
										end;
									end if;
								elsif Cmd = "target" then
									declare
										Target : constant Integer := Form.Get_Target (Inputs);
										Special : constant Boolean := Form.Get_Special (Inputs);
										Target_Day : constant Natural := Village.Target_Day;
									begin
										if Village.People.Constant_Reference(Player).Element.Records.Constant_Reference(Village.Today).Element.State = Villages.Died then
											Message_Page ("あなたは死んでいます。");
										elsif Special and then Village.Silver_Bullet_State (Player) /= Villages.Allowed then
											Message_Page ("銀の弾丸は一発限りです。");
										elsif Village.Daytime_Preview /= Villages.None
											and then Village.People.Constant_Reference(Player).Element.Role in Villages.Daytime_Role
										then
											case Village.Superman_State (Player) is
												when Villages.Disallowed =>
													Message_Page ("医者と探偵は、夜に能力を使えません。");
												when Villages.Already_Used =>
													Message_Page ("医者と探偵の行動選択は一日に一度しかできません。");
												when Villages.Allowed =>
													if Target < 0 then
														Refresh_Page;
													else
														Web.Header_Content_Type (Output, Web.Text_HTML);
														Web.Header_Cookie (Output, Cookie, Now + Configurations.Cookie_Duration);
														Web.Header_Break (Output);
														R3.Target_Page (
															Output,
															Form,
															Configurations.Template_Names (Form.Template_Set).Template_Target_File_Name.all,
															Village_Id => Village_Id,
															Village => Village,
															Player => Player,
															Target => Target,
															User_Id => User_Id,
															User_Password => User_Password);
													end if;
											end case;
										else
											if Target /= Village.People.Constant_Reference(Player).Element.Records.Constant_Reference(Target_Day).Element.Target
												or else Special /= Village.People.Constant_Reference(Player).Element.Records.Constant_Reference(Target_Day).Element.Special
											then
												Villages.Select_Target (Village, Player, Target, Special, Now);
												Villages.Save (Tabula.Villages.Lists.File_Name (Village_List, Village_Id), Village);
											end if;
											Refresh_Page;
										end if;
									end;
								elsif Cmd = "target2" then
									if Village.People.Constant_Reference(Player).Element.Records.Constant_Reference(Village.Today).Element.State = Villages.Died then
										Message_Page ("あなたは死んでいます。");
									elsif Village.People.Constant_Reference (Player).Element.Role in Villages.Daytime_Role then
										case Village.Superman_State (Player) is
											when Villages.Disallowed =>
												Message_Page ("医者と探偵は、夜に能力を使えません。");
											when Villages.Already_Used =>
												Message_Page ("医者と探偵の行動選択は一日に一度しかできません。");
											when Villages.Allowed =>
												declare
													Target : constant Integer := Form.Get_Target (Inputs);
												begin
													Villages.Select_Target (Village, Player, Target, False, Now);
													Villages.Save (Tabula.Villages.Lists.File_Name (Village_List, Village_Id), Village);
													Refresh_Page;
												end;
										end case;
									else
										Message_Page ("医者と探偵以外は、日中に能力を使えません。");
									end if;
								elsif Cmd = "rule" then
									if Village.Today > 0 then
										Message_Page ("開始以降ルールは変更できません。");
									else
										Forms.Set_Rule (Form, Village, Inputs);
										Villages.Save (Tabula.Villages.Lists.File_Name (Village_List, Village_Id), Village);
										Refresh_Page;
									end if;
								else
									Message_Page ("不正なコマンド(" & Cmd & ")が送られました。");
								end if;
							end Commands;
						end if;
					end Village_Page;
				end if;
			end Valid_User;
		end if;
	end Locked;
exception
	when Web.Lock_Files.Lock_Error =>
		Web.Header_Content_Type (Output, Web.Text_Plain);
		Web.Header_Break(Output);
		declare
			Message : constant String := "White fog. Wait 1 minute!";
		begin
			Ada.Debug.Put (Message);
			String'Write(Output, Message);
			Character'Write (Output, ASCII.LF);
		end;
	when E : others =>
		Web.Header_Content_Type (Output, Web.Text_Plain);
		Web.Header_Break(Output);
		declare
			Message : constant String := Ada.Exceptions.Exception_Information (E);
		begin
			Ada.Debug.Put (Message);
			String'Write (Output, Message);
			Character'Write (Output, ASCII.LF);
		end;
end Vampire.Main;
