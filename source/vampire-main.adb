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
with Vampire.Configurations.Templates;
with Vampire.Forms.Select_Form;
with Vampire.Renderers.Error_Page;
with Vampire.Renderers.Index_Page;
with Vampire.Renderers.Message_Page;
with Vampire.Renderers.Preview_Page;
with Vampire.Renderers.Register_Page;
with Vampire.Renderers.Target_Page;
with Vampire.Renderers.User_Page;
with Vampire.Renderers.Users_Page;
with Vampire.Renderers.Village_Page;
with Vampire.Renderers.Simple;
with Vampire.Renderers.Log;
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
	use type Villages.Attack_Mode;
	use type Villages.Doctor_Infected_Mode;
	use type Villages.Daytime_Preview_Mode;
	use type Villages.Person_Role;
	use type Villages.Person_State;
	use type Villages.Message_Kind;
	use type Villages.Message;
	use type Villages.Village_Time;
	
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
	Users_List : Users.Lists.Users_List := Users.Lists.Create (
		Directory => Configurations.Users_Directory'Access,
		Log_File_Name => Configurations.Users_Log_File_Name'Access);
	
	-- 村情報
	Villages_List : Tabula.Villages.Lists.Villages_List := Tabula.Villages.Lists.Create (
		Data_Directory => Configurations.Villages_Data_Directory'Access,
		HTML_Directory => Configurations.Villages_HTML_Directory'Access,
		Blocking_Short_Term_File_Name => Configurations.Villages_Blocking_Short_Term_File_Name'Access,
		Cache_File_Name => Configurations.Villages_Cache_File_Name'Access,
		Create_Index => Renderers.Log.Create_Index'Access,
		Types => (
			1 => (
				Type_Code => Renderers.Log.Type_Code'Access,
				Load_Summary => Renderers.Log.Load_Summary'Access,
				Create_Log => Renderers.Log.Create_Log'Access)));
	
begin
	Debug.Hook (Configurations.Debug_Log_File_Name'Access, Now);
	Ada.Environment_Variables.Set ("TMPDIR", Configurations.Temporary_Directory);
	declare
		function Get_Renderer(Query_Strings : in Web.Query_Strings) return Renderers.Renderer'Class is
		begin
			if Web.Element (Query_Strings, "b") = "k" then
				return Renderers.Simple.Renderer'(Configuration => Configurations.Templates.Simple_Configuration);
			else
				return Renderers.Renderer'(Configuration => Configurations.Templates.Configuration);
			end if;
		end Get_Renderer;
		Lock : Web.Lock_Files.Lock_Type := Web.Lock_Files.Lock (Configurations.Lock_Name, Force => 60.0);
		-- HTTP Info
		Remote_Addr : String renames Web.Remote_Addr;
		Remote_Host : String renames Web.Remote_Host;
		Inputs : Web.Query_Strings renames Web.Get (Input);
		Query_Strings : Web.Query_Strings renames Web.Get_Query_Strings;
		Cookie : Web.Cookie := Web.Get_Cookie; -- variable
		Post : Boolean renames Web.Post;
		-- Values
		pragma Warnings (Off); -- compiler...
		Form : Forms.Root_Form_Type'Class := Forms.Select_Form (Query_Strings);
		pragma Warnings (On);
		Renderer : Renderers.Renderer'Class renames Get_Renderer(Query_Strings);
		User_Id : constant String := Form.Get_User_Id (Query_Strings, Cookie);
		User_Password : constant String := Form.Get_User_Password (Query_Strings, Cookie);
		Village_Id : constant Tabula.Villages.Village_Id := Form.Get_Village_Id (Query_Strings);
		Cmd : constant String := Form.Get_Command (Inputs);
		procedure Render_Reload_Page is
		begin
			Web.Header_See_Other (Output, Web.Request_URI);
			Web.Header_Content_Type (Output, Web.Text_HTML);
			Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
			Web.Header_Break (Output);
			Renderer.Refresh_Page (Output, URI => Web.Request_URI);
		end Render_Reload_Page;
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
			declare
				New_User_Id : constant String := Form.Get_New_User_Id (Inputs);
				New_User_Password : constant String := Form.Get_New_User_Password (Inputs);
				User_State : Users.Lists.User_State;
				User_Info : Users.User_Info;
			begin
				Users.Lists.Query (Users_List,
					Id => New_User_Id, Password => New_User_Password,
					Remote_Addr => Remote_Addr, Remote_Host => Remote_Host,
					Now => Now,
					Info => User_Info, State => User_State);
				case User_State is
					when Users.Lists.Log_Off =>
						Web.Header_Content_Type (Output, Web.Text_HTML);
						Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
						Web.Header_Break (Output);
						Renderer.Message_Page(Output,
							Village_Id => Village_Id, Message => "IDを入力してください。",
							User_Id => "", User_Password => "");
					when Users.Lists.Unknown =>
						if Users.Valid_Id_String(New_User_Id) then
							Web.Header_Content_Type (Output, Web.Text_HTML);
							Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
							Web.Header_Break (Output);
							Renderer.Register_Page(Output,
								Village_Id => Village_Id,
								New_User_Id => New_User_Id, New_User_Password => New_User_Password);
						else
							Web.Header_Content_Type (Output, Web.Text_HTML);
							Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
							Web.Header_Break (Output);
							Renderer.Message_Page(Output,
								Village_Id => Village_Id, Message => "変な文字は使わないでください。",
								User_Id => "", User_Password => "");
						end if;
					when Users.Lists.Invalid =>
						Web.Header_Content_Type (Output, Web.Text_HTML);
						Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
						Web.Header_Break (Output);
						Renderer.Message_Page(Output,
							Village_Id => Village_Id, Message => "パスワードが異なります。",
							User_Id => "", User_Password => "");
					when Users.Lists.Valid =>
						Forms.Set_User (
							Form,
							Cookie,
							New_User_Id => New_User_Id,
							New_User_Password => New_User_Password);
						Web.Header_Content_Type (Output, Web.Text_HTML);
						Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
						Web.Header_Break (Output);
						Renderer.Message_Page(Output,
							Village_Id => Village_Id, Message => "ログオンしました。",
							User_Id => New_User_Id, User_Password => New_User_Password);
						Users.Lists.Update (Users_List,
							Id => New_User_Id,
							Remote_Addr => Remote_Addr,
							Remote_Host => Remote_Host,
							Now => Now,
							Info => User_Info);
				end case;
			end;
		elsif Cmd = "logoff" then
			declare
				User_State : Users.Lists.User_State;
				User_Info : Users.User_Info;
			begin
				Users.Lists.Query (Users_List,
					Id => User_Id, Password => User_Password,
					Remote_Addr => Remote_Addr, Remote_Host => Remote_Host,
					Now => Now,
					Info => User_Info, State => User_State);
				case User_State is
					when Users.Lists.Valid =>
						Users.Lists.Update (Users_List,
							Id => User_Id,
							Remote_Addr => Remote_Addr,
							Remote_Host => Remote_Host,
							Now => Now,
							Info => User_Info);
					when others =>
						null;
				end case;
			end;
			Forms.Set_User (
				Form,
				Cookie,
				New_User_Id => "",
				New_User_Password => "");
			Web.Header_Content_Type (Output, Web.Text_HTML);
			Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
			Web.Header_Break (Output);
			Renderer.Message_Page(Output,
				Village_Id => Village_Id, Message => "ログオフしました。",
				User_Id => "", User_Password => "");
		elsif Cmd = "register" then
			declare
				New_User_Id : constant String := Form.Get_New_User_Id (Inputs);
				New_User_Password : constant String := Form.Get_New_User_Password (Inputs);
				New_User_Password_Retype : constant String := Form.Get_New_User_Confirmation_Password (Inputs);
				Registered : Boolean;
			begin
				if New_User_Password /= New_User_Password_Retype then
					Web.Header_Content_Type (Output, Web.Text_HTML);
					Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
					Web.Header_Break (Output);
					Renderer.Message_Page(Output,
						Village_Id => Village_Id, Message => "再入力されたパスワードが異なります。",
						User_Id => "", User_Password => "");
				else
					Users.Lists.New_User (Users_List,
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
						Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
						Web.Header_Break (Output);
						Renderer.Message_Page(Output,
							Village_Id => Village_Id, Message => "登録しました。",
							User_Id => New_User_Id, User_Password => New_User_Password);
					else
						Web.Header_Content_Type (Output, Web.Text_HTML);
						Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
						Web.Header_Break (Output);
						Renderer.Message_Page(Output,
							Village_Id => Village_Id, Message => "登録に失敗しました。",
							User_Id => "", User_Password => "");
					end if;
				end if;
			end;
		elsif Cmd = "newl" or else Cmd = "news" then -- 村作成
			declare
				User_State : Users.Lists.User_State;
				User_Info : Users.User_Info;
				Term : Tabula.Villages.Village_Term;
				Summaries : Tabula.Villages.Lists.Summary_Maps.Map;
			begin
				Users.Lists.Query (Users_List,
					Id => User_Id, Password => User_Password,
					Remote_Addr => Remote_Addr, Remote_Host => Remote_Host,
					Now => Now,
					Info => User_Info, State => User_State);
				if Cmd = "news" then
					Term := Tabula.Villages.Short;
				else
					Term := Tabula.Villages.Long;
				end if;
				case User_State is
					when Users.Lists.Valid =>
						Tabula.Villages.Lists.Get_Summaries (Villages_List, Summaries);
						if User_Id /= Users.Administrator
							and then Tabula.Villages.Lists.Exists_Opened_By (Summaries, User_Id)
						then
							Web.Header_Content_Type (Output, Web.Text_HTML);
							Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
							Web.Header_Break (Output);
							Renderer.Message_Page(Output, Message => "同時に村をふたつ作成することはできません。",
								User_Id => User_Id, User_Password => User_Password);
						elsif Term = Tabula.Villages.Short
							and then (User_Info.Disallow_New_Village
								or else (
									Tabula.Villages.Lists.Blocking_Short_Term (Villages_List)
									and then User_Id /= Users.Administrator
									and then User_Id /= "she")) -- ハードコーディングですよ酷いコードですね
						then
							Web.Header_Content_Type (Output, Web.Text_HTML);
							Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
							Web.Header_Break (Output);
							Renderer.Message_Page(Output, Message => "えーと、しばらく短期は延期で。",
								User_Id => User_Id, User_Password => User_Password);
						else
							declare
								Village_Name : constant String := Form.Get_New_Village_Name (Inputs);
							begin
								if Village_Name = "" then
									Web.Header_Content_Type (Output, Web.Text_HTML);
									Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
									Web.Header_Break (Output);
									Renderer.Message_Page(Output,
										Message => "村名を入力してください。",
										User_Id => User_Id, User_Password => User_Password);
								else
									declare
										New_Village : Villages.Village_Type := Villages.Create (
											Name => Village_Name,
											By => User_Id,
											Term => Term,
											Time => Now);
										New_Village_Id : constant Tabula.Villages.Village_Id :=
											Tabula.Villages.Lists.New_Village_Id (Villages_List);
									begin
										Villages.Save (
											Tabula.Villages.Lists.File_Name (Villages_List, New_Village_Id),
											New_Village);
										Web.Header_Content_Type (Output, Web.Text_HTML);
										Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
										Web.Header_Break (Output);
										Renderer.Message_Page(Output,
											Message => "新たな村「" & Village_Name & "」を作成しました。",
											User_Id => User_Id, User_Password => User_Password);
										Tabula.Villages.Lists.Update (
											Villages_List, 
											New_Village_Id,
											Tabula.Villages.Lists.Summary (
												Renderers.Log.Type_Code,
												New_Village));
										Users.Lists.Update (Users_List,
											Id => User_Id,
											Remote_Addr => Remote_Addr,
											Remote_Host => Remote_Host,
											Now => Now,
											Info => User_Info);
									exception
										when Ada.IO_Exceptions.Name_Error =>
											Web.Header_Content_Type (Output, Web.Text_HTML);
											Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
											Web.Header_Break (Output);
											Renderer.Message_Page(Output,
												Message => "作成に失敗しました。",
												User_Id => User_Id, User_Password => User_Password);
									end;
								end if;
							end;
						end if;
					when others =>
						Web.Header_Content_Type (Output, Web.Text_HTML);
						Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
						Web.Header_Break (Output);
						Renderer.Error_Page(Output, "正常にログオンしてください。");
				end case;
			end;
		elsif Cmd = "remakelog" then
			declare
				User_State : Users.Lists.User_State;
				User_Info : Users.User_Info;
			begin
				Users.Lists.Query (Users_List,
					Id => User_Id, Password => User_Password,
					Remote_Addr => Remote_Addr, Remote_Host => Remote_Host,
					Now => Now,
					Info => User_Info, State => User_State);
				if User_State = Users.Lists.Valid and then User_Id = Tabula.Users.Administrator then
					Tabula.Villages.Lists.Refresh (Villages_List);
					Render_Reload_Page;
				else
					Web.Header_Content_Type (Output, Web.Text_HTML);
					Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
					Web.Header_Break (Output);
					Renderer.Error_Page(Output, "administratorのみに許された操作です。");
				end if;
			end;
		elsif Village_Id = Tabula.Villages.Invalid_Village_Id then
			if Cmd = "" then
				if Post then
					Render_Reload_Page;
				elsif Form.Is_User_Page (Query_Strings, Cookie) then
					declare
						User_State : Users.Lists.User_State;
						User_Info : Users.User_Info;
						Summaries : Tabula.Villages.Lists.Summary_Maps.Map;
					begin
						Users.Lists.Query (Users_List,
							Id => User_Id, Password => User_Password,
							Remote_Addr => Remote_Addr, Remote_Host => Remote_Host,
							Now => Now,
							Info => User_Info, State => User_State);
						if User_State = Users.Lists.Valid and then User_Id /= Users.Administrator then
							Web.Header_Content_Type (Output, Web.Text_HTML);
							Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
							Web.Header_Break (Output);
							Tabula.Villages.Lists.Get_Summaries (Villages_List, Summaries);
							Renderer.User_Page (
								Output,
								Summaries,
								User_Id => User_Id,
								User_Password => User_Password,
								User_Info => User_Info);
						else
							Web.Header_Content_Type (Output, Web.Text_HTML);
							Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
							Web.Header_Break (Output);
							Renderer.Error_Page(Output, "正常にログオンしないとユーザーページは表示できません。");
						end if;
					end;
				elsif Form.Is_User_List_Page (Query_Strings) then
					Web.Header_Content_Type (Output, Web.Text_HTML);
					Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
					Web.Header_Break (Output);
					declare
						Summaries : Tabula.Villages.Lists.Summary_Maps.Map;
					begin
						Tabula.Villages.Lists.Get_Summaries (Villages_List, Summaries);
						Renderers.Users_Page (
							Renderer,
							Output,
							Summaries,
							User_List => Users.Lists.All_Users (Users_List),
							User_Id => User_Id,
							User_Password => User_Password);
					end;
				else
					Web.Header_Content_Type (Output, Web.Text_HTML);
					Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
					Web.Header_Break (Output);
					declare
						Summaries : Tabula.Villages.Lists.Summary_Maps.Map;
					begin
						Tabula.Villages.Lists.Get_Summaries (Villages_List, Summaries);
						Renderer.Index_Page (
							Output,
							Summaries,
							Users.Lists.Muramura_Count (Users_List, Now, Muramura_Duration),
							User_Id => User_Id,
							User_Password => User_Password);
					end;
				end if;
			else
				Web.Header_Content_Type (Output, Web.Text_HTML);
				Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
				Web.Header_Break (Output);
				Renderer.Error_Page(Output, "不正なコマンド(" & Cmd & ")が送られました。");
			end if;
		else
			declare
				User_State : Users.Lists.User_State;
				User_Info : Users.User_Info;
			begin
				Users.Lists.Query (Users_List,
					Id => User_Id, Password => User_Password,
					Remote_Addr => Remote_Addr, Remote_Host => Remote_Host,
					Now => Now,
					Info => User_Info, State => User_State);
				if User_State = Users.Lists.Invalid then
					Web.Header_Content_Type (Output, Web.Text_HTML);
					Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
					Web.Header_Break (Output);
					Renderer.Error_Page(Output, "パスワードが不正です。");
				elsif not Tabula.Villages.Lists.Exists (Villages_List, Village_Id) then
					Web.Header_Content_Type (Output, Web.Text_HTML);
					Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
					Web.Header_Break (Output);
					Renderer.Error_Page(Output, "存在しない村が指定されました。");
				else
					declare
						Village : aliased Villages.Village_Type;
					begin
						Villages.Load (Tabula.Villages.Lists.File_Name (Villages_List, Village_Id), Village);
						-- Village.Name := +Village.Name.Constant_Reference.Element.all; -- dirty hack for memory bug
						if Cmd = "" then
							if Post then
								Render_Reload_Page;
							else
								-- 強制進行
								declare
									Changed, List_Changed : Boolean;
								begin
									Villages.Advance(Village, Now, Generator'Access,
										Changed => Changed, List_Changed => List_Changed);
									if Changed then
										Villages.Save (Tabula.Villages.Lists.File_Name (Villages_List, Village_Id), Village);
									end if;
									if List_Changed then
										Tabula.Villages.Lists.Update (
											Villages_List,
											Village_Id,
											Tabula.Villages.Lists.Summary (
												Renderers.Log.Type_Code,
												Village));
									end if;
								end;
								-- 村レンダリング
								declare
									Day : Natural := Form.Get_Day (Village, Query_Strings);
									Message_Range : Forms.Message_Range := Form.Get_Range (Village, Day, Query_Strings);
								begin
									Web.Header_Content_Type (Output, Web.Text_HTML);
									Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
									Web.Header_Break (Output);
									Renderers.Village_Page(
										Renderer,
										Output,
										Village_Id,
										Village'Access,
										Day => Day,
										First => Message_Range.First,
										Last => Message_Range.Last,
										User_Id => User_Id,
										User_Password => User_Password);
								end;
							end if;
						elsif Village.State = Tabula.Villages.Closed then
							Web.Header_Content_Type (Output, Web.Text_HTML);
							Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
							Web.Header_Break (Output);
							Renderer.Error_Page(Output, "終了した村にコマンドが送られました。");
						else
							declare
								Player : Integer := Villages.Joined(Village, User_Id);
								function Speech_Check return Boolean is
								begin
									if Village.State = Tabula.Villages.Playing
										and then Village.People.Constant_Reference(Player).Element.Records.Constant_Reference(Village.Today).Element.State = Villages.Died
									then
										Web.Header_Content_Type (Output, Web.Text_HTML);
										Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
										Web.Header_Break (Output);
										Renderers.Message_Page(Renderer, Output, Village_Id, Village'Access, "あなたは死にました。", User_Id, User_Password);
										return False;
									elsif Village.Time /= Villages.Daytime then
										Web.Header_Content_Type (Output, Web.Text_HTML);
										Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
										Web.Header_Break (Output);
										Renderers.Message_Page(Renderer, Output,
											Village_Id, Village'Access, "今は喋れない時間帯です。", User_Id, User_Password);
										return False;
									else
										return True;
									end if;
								end Speech_Check;
								Summaries : Tabula.Villages.Lists.Summary_Maps.Map;
							begin
								Tabula.Villages.Lists.Get_Summaries (Villages_List, Summaries);
								if User_State /= Users.Lists.Valid then
									Web.Header_Content_Type (Output, Web.Text_HTML);
									Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
									Web.Header_Break (Output);
									Renderer.Error_Page(Output, "正常にログオンしてください。");
								elsif Cmd = "join" then
									if Player >= 0 then
										Web.Header_Content_Type (Output, Web.Text_HTML);
										Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
										Web.Header_Break (Output);
										Renderer.Error_Page(Output, "既にこの村に参加しています。");
									elsif Village.State /= Tabula.Villages.Prologue then
										Web.Header_Content_Type (Output, Web.Text_HTML);
										Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
										Web.Header_Break (Output);
										Renderer.Message_Page(Output, Village_Id, Village'Access, "締め切りです。", User_Id, User_Password);
									elsif Tabula.Villages.Lists.Exists_Opened_By (Summaries, User_Id, Excluding => Village_Id) then
										Web.Header_Content_Type (Output, Web.Text_HTML);
										Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
										Web.Header_Break (Output);
										Renderer.Message_Page(Output, Village_Id, Village'Access, "自分の作成した村に入ってください。", User_Id, User_Password);
									elsif Village.Day_Duration >= 24 * 60 * 60.0
										and then Tabula.Villages.Lists.Count_Joined_By (
											Summaries,
											User_Id,
											Filter => (
												Tabula.Villages.Prologue | Tabula.Villages.Playing => True,
												Tabula.Villages.Epilogue | Tabula.Villages.Closed => False),
											Long_Only => True) > 0
									then
										Web.Header_Content_Type (Output, Web.Text_HTML);
										Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
										Web.Header_Break (Output);
										Renderer.Message_Page(Output, Village_Id, Village'Access, "既に他の村に参加しています。", User_Id, User_Password);
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
													Web.Header_Content_Type (Output, Web.Text_HTML);
													Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
													Web.Header_Break (Output);
													Renderer.Message_Page(Output, Village_Id, Village'Access,
														"申し訳ありませんが既定の肩書き(" & Person_Template.Work.Constant_Reference.Element.all & ")は既に取られています。",
														User_Id, User_Password);
												else
													declare
														Selected_Work : Casts.Work
															renames Cast.Works.Constant_Reference (Joining.Work_Index).Element.all;
													begin
														if Person_Template.Name = "" or Selected_Work.Name = "" then
															Web.Header_Content_Type (Output, Web.Text_HTML);
															Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
															Web.Header_Break (Output);
															Renderer.Message_Page(Output, Village_Id, Village'Access, "既に取られています。", User_Id, User_Password);
														elsif Selected_Work.Sex /= Casts.Neutral and then Selected_Work.Sex /= Person_Template.Sex then
															Web.Header_Content_Type (Output, Web.Text_HTML);
															Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
															Web.Header_Break (Output);
															Renderer.Message_Page(Output, Village_Id, Village'Access, "性別と肩書きが一致しません。", User_Id, User_Password);
														elsif Selected_Work.Nominated and then Selected_Work.Name /= Person_Template.Work then
															Web.Header_Content_Type (Output, Web.Text_HTML);
															Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
															Web.Header_Break (Output);
															Renderer.Message_Page(Output, Village_Id, Village'Access, "その肩書きは特定の組み合わせでしか使えません。", User_Id, User_Password);
														elsif Villages.Already_Joined_As_Another_Sex(Village, User_Id, Person_Template.Sex) then
															Web.Header_Content_Type (Output, Web.Text_HTML);
															Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
															Web.Header_Break (Output);
															Renderer.Message_Page(Output, Village_Id, Village'Access, "以前にエントリされたときと性別が異なります。", User_Id, User_Password);
														else
															Villages.Join (
																Village,
																User_Id,
																Person_Template,
																Selected_Work,
																Joining.Request,
																User_Info.Ignore_Request,
																Now);
															Villages.Save (Tabula.Villages.Lists.File_Name (Villages_List, Village_Id), Village);
															Tabula.Villages.Lists.Update (
																Villages_List,
																Village_Id,
																Tabula.Villages.Lists.Summary (
																	Renderers.Log.Type_Code,
																	Village));
															Web.Header_Content_Type (Output, Web.Text_HTML);
															Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
															Web.Header_Break (Output);
															Renderer.Message_Page(Output, Village_Id, Village'Access, "村に参加しました。", User_Id, User_Password);
														end if;
													end;
												end if;
											end;
										end;
									end if;
								elsif Cmd = "narration" then
									if User_Id /= Tabula.Users.Administrator then
										Web.Header_Content_Type (Output, Web.Text_HTML);
										Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
										Web.Header_Break (Output);
										Renderer.Error_Page(Output, "administratorのみに許された操作です。");
									else
										declare
											Text : constant String := Form.Get_Text (Inputs);
										begin
											Villages.Narration (Village, Text, Now);
											Villages.Save (
												Tabula.Villages.Lists.File_Name (Villages_List, Village_Id),
												Village);
										end;
										Render_Reload_Page;
									end if;
								elsif Cmd = "remove" then
									if Village.State /= Tabula.Villages.Prologue then
										Web.Header_Content_Type (Output, Web.Text_HTML);
										Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
										Web.Header_Break (Output);
										Renderer.Error_Page(Output, "除名を行えるのはプロローグのみです。");
									elsif User_Id /= Tabula.Users.Administrator then
										Web.Header_Content_Type (Output, Web.Text_HTML);
										Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
										Web.Header_Break (Output);
										Renderer.Error_Page(Output, "administratorのみに許された操作です。");
									else
										declare
											Target : constant Integer := Form.Get_Target (Inputs);
											Removed_Id : constant String :=
												Village.People.Constant_Reference (Target).Element.
													Id.Constant_Reference.Element.all;
										begin
											Villages.Escape(Village, Target, Now);
											Villages.Save (
												Tabula.Villages.Lists.File_Name (Villages_List, Village_Id),
												Village);
											Tabula.Villages.Lists.Update (
												Villages_List,
												Village_Id,
												Tabula.Villages.Lists.Summary (
													Renderers.Log.Type_Code,
													Village));
											Web.Header_Content_Type (Output, Web.Text_HTML);
											Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
											Web.Header_Break (Output);
											Renderers.Message_Page(Renderer, Output,
												Village_Id, Village'Access,
												Removed_Id & "を村から除名しました。",
												User_Id, User_Password);
										end;
									end if;
								elsif Player < 0 then
									Web.Header_Content_Type (Output, Web.Text_HTML);
									Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
									Web.Header_Break (Output);
									Renderer.Error_Page(Output, "参加していません。");
								elsif Cmd = "commit" then
									if Village.Time = Villages.Night then
										Web.Header_Content_Type (Output, Web.Text_HTML);
										Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
										Web.Header_Break (Output);
										Renderers.Message_Page(Renderer, Output, Village_Id, Village'Access, "今は行動を終えられない時間帯です。", User_Id, User_Password);
									elsif Village.People.Constant_Reference(Player).Element.Records.Constant_Reference(Village.Today).Element.State = Villages.Died then
										Web.Header_Content_Type (Output, Web.Text_HTML);
										Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
										Web.Header_Break (Output);
										Renderer.Error_Page(Output, "死者は行動を決定できません。");
									else
										if not Village.People.Constant_Reference(Player).Element.Commited then
											Village.People.Reference(Player).Element.Commited := True;
											declare
												Changed, List_Changed : Boolean;
											begin
												Villages.Advance(Village, Now, Generator'Access,
													Changed => Changed, List_Changed => List_Changed);
												Villages.Save (Tabula.Villages.Lists.File_Name (Villages_List, Village_Id), Village);
												if List_Changed then
													Tabula.Villages.Lists.Update (
														Villages_List,
														Village_Id,
														Tabula.Villages.Lists.Summary (
															Renderers.Log.Type_Code,
															Village));
												end if;
											end;
										end if;
										Render_Reload_Page;
									end if;
								elsif Cmd = "rollback" then
									if Village.People.Constant_Reference(Player).Element.Commited then
										Village.People.Reference(Player).Element.Commited := False;
										Villages.Save (Tabula.Villages.Lists.File_Name (Villages_List, Village_Id), Village);
									end if;
									Render_Reload_Page;
								elsif Cmd = "escape" then
									if Village.State /= Tabula.Villages.Prologue then
										Web.Header_Content_Type (Output, Web.Text_HTML);
										Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
										Web.Header_Break (Output);
										Renderer.Error_Page(Output, "村から出られるのはプロローグのみです。");
									else
										case Form.Get_Answered (Inputs) is
											when Forms.Missing =>
												Web.Header_Content_Type (Output, Web.Text_HTML);
												Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
												Web.Header_Break (Output);
												Renderers.Message_Page(Renderer, Output, Village_Id, Village'Access, "答えを入力してください。", User_Id, User_Password);
											when Forms.NG =>
												Web.Header_Content_Type (Output, Web.Text_HTML);
												Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
												Web.Header_Break (Output);
												Renderers.Message_Page(Renderer, Output, Village_Id, Village'Access, "答えが違います。", User_Id, User_Password);
											when Forms.OK =>
												Villages.Escape(Village, Player, Now);
												Villages.Save (Tabula.Villages.Lists.File_Name (Villages_List, Village_Id), Village);
												Tabula.Villages.Lists.Update (
													Villages_List,
													Village_Id,
													Tabula.Villages.Lists.Summary (
														Renderers.Log.Type_Code,
														Village));
												Web.Header_Content_Type (Output, Web.Text_HTML);
												Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
												Web.Header_Break (Output);
												Renderers.Message_Page(Renderer, Output, Village_Id, Village'Access, "旅に出ました。", User_Id, User_Password);
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
											Web.Header_Content_Type (Output, Web.Text_HTML);
											Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
											Web.Header_Break (Output);
											Renderers.Message_Page(Renderer, Output, Village_Id, Village'Access, "対象と行動を選んでください。", User_Id, User_Password);
										elsif Action = "wake" then
											if Said(Player).Wake > 0 then
												Web.Header_Content_Type (Output, Web.Text_HTML);
												Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
												Web.Header_Break (Output);
												Renderers.Error_Page(Renderer, Output,
													"人を起こせるのは一日一度です。");
											elsif not Village.People.Constant_Reference(Target).Element.Commited then
												Web.Header_Content_Type (Output, Web.Text_HTML);
												Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
												Web.Header_Break (Output);
												Renderers.Message_Page(Renderer, Output, Village_Id, Village'Access, "相手はまだ行動を終えていません。", User_Id, User_Password);
											else
												Villages.Wake (Village, Player, Target, Now);
												Villages.Save (Tabula.Villages.Lists.File_Name (Villages_List, Village_Id), Village);
												Render_Reload_Page;
											end if;
										elsif Action = "encourage" then
											if Said(Player).Encourage > 0 then
												Web.Header_Content_Type (Output, Web.Text_HTML);
												Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
												Web.Header_Break (Output);
												Renderers.Error_Page(Renderer, Output,
													"話の続きを促せるのは一日一度です。");
											else
												Villages.Encourage (Village, Player, Target, Now);
												Villages.Save (Tabula.Villages.Lists.File_Name (Villages_List, Village_Id), Village);
												Render_Reload_Page;
											end if;
										elsif Action = "vampire_gaze" then
											if Village.People.Constant_Reference(Player).Element.Role not in Villages.Vampire_Role then
												Web.Header_Content_Type (Output, Web.Text_HTML);
												Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
												Web.Header_Break (Output);
												Renderers.Error_Page(Renderer, Output, "見つめられるのは吸血鬼だけです。");
											elsif Village.People.Constant_Reference(Target).Element.Role in Villages.Vampire_Role then
												Web.Header_Content_Type (Output, Web.Text_HTML);
												Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
												Web.Header_Break (Output);
												Renderers.Message_Page(Renderer, Output, Village_Id, Village'Access, "見つめようとした相手は吸血鬼です。", User_Id, User_Password);
											elsif Said(Player).Vampire_Gaze > 0 then
												Web.Header_Content_Type (Output, Web.Text_HTML);
												Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
												Web.Header_Break (Output);
												Renderers.Error_Page(Renderer, Output, "見つめられるのは一日一度です。");
											elsif Village.Time = Villages.Night then
												Web.Header_Content_Type (Output, Web.Text_HTML);
												Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
												Web.Header_Break (Output);
												Renderers.Message_Page(Renderer, Output, Village_Id, Village'Access, "夜は直接会話できます。", User_Id, User_Password);
											else
												Villages.Gaze (Village, Player, Target, Now);
												Villages.Save (Tabula.Villages.Lists.File_Name (Villages_List, Village_Id), Village);
												Render_Reload_Page;
											end if;
										else
											Web.Header_Content_Type (Output, Web.Text_HTML);
											Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
											Web.Header_Break (Output);
											Renderer.Error_Page(Output, "サポートされていない種類のアクションを行おうとしました。");
										end if;
									end;
								elsif Cmd = "speech" then
									if Speech_Check then
										declare
											Text : constant String := Form.Get_Text (Inputs);
										begin
											if Text'Length > 0 then
												Web.Header_Content_Type (Output, Web.Text_HTML);
												Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
												Web.Header_Break (Output);
												Renderers.Preview_Page(Renderer, Output,
													Village_Id,
													Village,
													Villages.Message'(
														Day => Village.Today,
														Time => Calendar.Null_Time,
														Kind => Villages.Speech,
														Subject => Player,
														Target => -1,
														Text => +Text),
													User_Id => User_Id,
													User_Password => User_Password);
											else
												Render_Reload_Page;
											end if;
										end;
									end if;
								elsif Cmd = "speech2" then
									if Speech_Check then
										declare
											Text : constant String := Form.Get_Text (Inputs);
										begin
											if Text'Length > 0 then
												Villages.Speech (Village, Player, Text, Now);
												Villages.Save (Tabula.Villages.Lists.File_Name (Villages_List, Village_Id), Village);
											end if;
										end;
										Render_Reload_Page;
									end if;
								elsif Cmd = "reedit" then
									case Form.Get_Reedit_Kind (Inputs) is
										when Villages.Speech =>
											if Speech_Check then
												declare
													Text : constant String := Form.Get_Text (Inputs);
													Day : Natural := Form.Get_Day (Village, Query_Strings);
													Message_Range : Forms.Message_Range := Form.Get_Range (Village, Day, Query_Strings);
												begin
													Web.Header_Content_Type (Output, Web.Text_HTML);
													Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
													Web.Header_Break (Output);
													Renderers.Village_Page(
														Renderer,
														Output,
														Village_Id,
														Village'Access,
														Day => Day,
														First => Message_Range.First,
														Last => Message_Range.Last,
														Editing_Text => Text,
														User_Id => User_Id,
														User_Password => User_Password);
												end;
											end if;
										when others =>
											-- 通常発言以外の再編集は未実装……
											Render_Reload_Page;
									end case;
								elsif Cmd = "monologue" then
									if Village.State = Tabula.Villages.Epilogue then
										Web.Header_Content_Type (Output, Web.Text_HTML);
										Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
										Web.Header_Break (Output);
										Renderer.Error_Page(Output, "エピローグでは独白は喋れません。");
									else
										declare
											Text : constant String := Form.Get_Text (Inputs);
										begin
											if Text'Length > Max_Length_Of_Message then
												Web.Header_Content_Type (Output, Web.Text_HTML);
												Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
												Web.Header_Break (Output);
												Renderer.Preview_Page(Output,
													Village_Id,
													Village,
													Villages.Message'(
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
													Villages.Save (Tabula.Villages.Lists.File_Name (Villages_List, Village_Id), Village);
												end if;
												Render_Reload_Page;
											end if;
										end;
									end if;
								elsif Cmd = "ghost" then
									if Village.State = Tabula.Villages.Epilogue then
										Web.Header_Content_Type (Output, Web.Text_HTML);
										Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
										Web.Header_Break (Output);
										Renderer.Error_Page(Output, "エピローグでは呻けません。");
									elsif Village.People.Constant_Reference(Player).Element.Records.Constant_Reference(Village.Today).Element.State /= Villages.Died then
										Web.Header_Content_Type (Output, Web.Text_HTML);
										Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
										Web.Header_Break (Output);
										Renderer.Error_Page(Output, "生者は呻けません。");
									else
										declare
											Text : constant String := Form.Get_Text (Inputs);
										begin
											if Text'Length > Max_Length_Of_Message then
												Web.Header_Content_Type (Output, Web.Text_HTML);
												Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
												Web.Header_Break (Output);
												Renderer.Preview_Page(Output,
													Village_Id,
													Village,
													Villages.Message'(
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
													Villages.Save (Tabula.Villages.Lists.File_Name (Villages_List, Village_Id), Village);
												end if;
												Render_Reload_Page;
											end if;
										end;
									end if;
								elsif Cmd = "note" then
									declare
										Text : constant String := Form.Get_Text (Inputs);
									begin
										if Text'Length > Max_Length_Of_Message then
											Web.Header_Content_Type (Output, Web.Text_HTML);
											Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
											Web.Header_Break (Output);
											Renderers.Preview_Page(Renderer, Output,
												Village_Id,
												Village,
												Villages.Message'(
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
												Villages.Save (Tabula.Villages.Lists.File_Name (Villages_List, Village_Id), Village);
											end if;
											Render_Reload_Page;
										end if;
									end;
								elsif Cmd = "vote" then
									if Village.Time = Villages.Night then
										Web.Header_Content_Type (Output, Web.Text_HTML);
										Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
										Web.Header_Break (Output);
										Renderer.Message_Page(Output, Village_Id, Village'Access, "今は投票できない時間帯です。", User_Id, User_Password);
									else
										declare
											Target : constant Integer := Form.Get_Target (Inputs);
										begin
											if Village.People.Constant_Reference(Player).Element.Records.Constant_Reference(Village.Today).Element.State = Villages.Died then
												Web.Header_Content_Type (Output, Web.Text_HTML);
												Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
												Web.Header_Break (Output);
												Renderer.Error_Page(Output, "あなたは死んでいます。");
											elsif Target >= 0 and then Village.People.Constant_Reference(Target).Element.Records.Constant_Reference(Village.Today).Element.State = Villages.Died then
												Web.Header_Content_Type (Output, Web.Text_HTML);
												Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
												Web.Header_Break (Output);
												Renderer.Error_Page(Output, "死者に投票はできません。");
											elsif Target >= 0 and then not Village.People.Constant_Reference(Target).Element.Records.Constant_Reference(Village.Today).Element.Candidate then
												Web.Header_Content_Type (Output, Web.Text_HTML);
												Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
												Web.Header_Break (Output);
												Renderer.Message_Page(Output, Village_Id, Village'Access, "仮投票が行われました。選ばれた候補以外に投票はできなくなります。", User_Id, User_Password);
											else
												if Target /= Village.People.Constant_Reference(Player).Element.Records.Constant_Reference(Village.Today).Element.Vote then
													Villages.Vote(Village, Player, Target);
													Villages.Save (Tabula.Villages.Lists.File_Name (Villages_List, Village_Id), Village);
												end if;
												Render_Reload_Page;
											end if;
										end;
									end if;
								elsif Cmd = "target" then
									declare
										Target : constant Integer := Form.Get_Target (Inputs);
										Special : constant Boolean := Form.Get_Special (Inputs);
										Target_Day : constant Natural := Village.Target_Day;
										Special_Used : constant Boolean := Village.Already_Used_Special (Player);
									begin
										if Special_Used and Special then
											Web.Header_Content_Type (Output, Web.Text_HTML);
											Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
											Web.Header_Break (Output);
											Renderer.Error_Page(Output, "銀の弾丸は一発限りです。");
										elsif Village.Daytime_Preview /= Villages.None
											and then Village.People.Constant_Reference(Player).Element.Role in Villages.Daytime_Role
										then
											if Village.Time = Villages.Night then
												Web.Header_Content_Type (Output, Web.Text_HTML);
												Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
												Web.Header_Break (Output);
												Renderer.Message_Page(Output, Village_Id, Village'Access, "医者と探偵は、夜に能力を使えません。", User_Id, User_Password);
											elsif Village.People.Constant_Reference(Player).Element.Records.Constant_Reference(Target_Day).Element.Target >= 0 then
												Web.Header_Content_Type (Output, Web.Text_HTML);
												Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
												Web.Header_Break (Output);
												Renderer.Error_Page(Output, "医者と探偵の行動選択は一日に一度しかできません。");
											elsif Target < 0 then
												Render_Reload_Page;
											else
												Web.Header_Content_Type (Output, Web.Text_HTML);
												Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
												Web.Header_Break (Output);
												Renderers.Target_Page(Renderer, Output,
													Village_Id,
													Village,
													Player => Player,
													Target => Target,
													User_Id => User_Id,
													User_Password => User_Password);
											end if;
										else
											if Target /= Village.People.Constant_Reference(Player).Element.Records.Constant_Reference(Target_Day).Element.Target
												or else Special /= Village.People.Constant_Reference(Player).Element.Records.Constant_Reference(Target_Day).Element.Special
											then
												Villages.Select_Target (Village, Player, Target, Special, Now);
												Villages.Save (Tabula.Villages.Lists.File_Name (Villages_List, Village_Id), Village);
											end if;
											Render_Reload_Page;
										end if;
									end;
								elsif Cmd = "target2" then
									if Village.Time = Villages.Night then
										Web.Header_Content_Type (Output, Web.Text_HTML);
										Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
										Web.Header_Break (Output);
										Renderer.Message_Page(Output, Village_Id, Village'Access, "医者と探偵は、夜に能力を使えません。", User_Id, User_Password);
									else
										declare
											Target : constant Integer := Form.Get_Target (Inputs);
										begin
											case Village.People.Constant_Reference (Player).Element.Role is
												when Villages.Doctor | Villages.Detective =>
													Villages.Select_Target (Village, Player, Target, False, Now);
													Villages.Save (Tabula.Villages.Lists.File_Name (Villages_List, Village_Id), Village);
													Render_Reload_Page;
												when others =>
													Web.Header_Content_Type (Output, Web.Text_HTML);
													Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
													Web.Header_Break (Output);
													Renderer.Error_Page(Output, "医者と探偵以外は、日中に能力を使えません。");
											end case;
										end;
									end if;
								elsif Cmd = "rule" then
									if Village.Today > 0 then
										Web.Header_Content_Type (Output, Web.Text_HTML);
										Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
										Web.Header_Break (Output);
										Renderer.Message_Page(Output, Village_Id, Village'Access, "開始以降ルールは変更できません。", User_Id, User_Password);
									else
										Forms.Set_Rule (Form, Village, Inputs);
										Villages.Save (Tabula.Villages.Lists.File_Name (Villages_List, Village_Id), Village);
										Render_Reload_Page;
									end if;
								else
									Web.Header_Content_Type (Output, Web.Text_HTML);
									Web.Header_Break(Output);
									Renderer.Error_Page(Output, "不正なコマンドが送られました。");
								end if;
							end;
						end if;
					end;
				end if;
			end;
		end if;
	end;
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
