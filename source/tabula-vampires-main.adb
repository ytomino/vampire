-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Calendar;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Numerics.MT19937;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Text_Streams;
with Web.Lock_Files;
with Tabula.Calendar;
with Tabula.Configurations.Templates;
with Tabula.Debug;
with Tabula.Renderers.Error_Page;
with Tabula.Renderers.Index_Page;
with Tabula.Renderers.Message_Page;
with Tabula.Renderers.Preview_Page;
with Tabula.Renderers.Register_Page;
with Tabula.Renderers.Target_Page;
with Tabula.Renderers.User_Page;
with Tabula.Renderers.Users_Page;
with Tabula.Renderers.Village_Page;
with Tabula.Renderers.Simple;
with Tabula.Renderers.Log;
with Tabula.Users.Lists;
with Tabula.Casts.Load;
with Tabula.Vampires.Villages.Advance;
with Tabula.Vampires.Villages.Load;
with Tabula.Vampires.Villages.Save;
with Tabula.Villages.Lists;
procedure Tabula.Vampires.Main is
	use type Ada.Calendar.Time;
	use type Ada.Strings.Unbounded.Unbounded_String;
	use type Casts.Person_Sex;
	use type Casts.Work;
	use type Users.Lists.User_State;
	use type Villages.Attack_Mode;
	use type Villages.Doctor_Infected_Mode;
	use type Villages.Daytime_Preview_Mode;
	use type Villages.Person_Role;
	use type Villages.Person_State;
	use type Villages.Message_Kind;
	use type Villages.Message;
	use type Tabula.Villages.Village_State;
	use type Tabula.Villages.Village_Time;
	use Villages.Messages;
	use Villages.Person_Records;
	use Villages.People;
	
	function "+" (S : Ada.Strings.Unbounded.Unbounded_String) return String renames Ada.Strings.Unbounded.To_String;
	
	Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
	
	procedure Add(
		Village : in out Villages.Village_Type;
		Kind : Villages.Message_Kind;
		Subject : Integer := -1;
		Target : Integer := -1;
		Text : String := "")
	is
		New_Item : Villages.Message := (
			Day => Village.Today,
			Time => Now,
			Kind => Kind,
			Subject => Subject,
			Target => Target,
			Text => +Text);
	begin
		if Village.Messages.Is_Empty or else Village.Messages.Last_Element /= New_Item then
			Append(Village.Messages, New_Item);
		end if;
	end Add;
	
	function Get_Renderer(Query_Strings : in Web.Query_Strings) return Renderers.Renderer'Class is
	begin
		if Web.Element (Query_Strings, "b") = "k" then
			return Renderers.Simple.Renderer'(Configuration => Configurations.Templates.Simple_Configuration);
		else
			return Renderers.Renderer'(Configuration => Configurations.Templates.Configuration);
		end if;
	end Get_Renderer;
	
	-- 標準入出力
	Input : not null Ada.Text_IO.Text_Streams.Stream_Access :=
		Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Standard_Input.all);
	Output : not null Ada.Text_IO.Text_Streams.Stream_Access :=
		Ada.Text_IO.Text_Streams.Stream (Ada.Text_IO.Standard_Output.all);
	
	-- 乱数シード
	Generator : aliased Ada.Numerics.MT19937.Generator := Ada.Numerics.MT19937.Initialize;
	
	-- ユーザー情報
	Users_List : Users.Lists.Users_List := Users.Lists.Create (
		Directory => Configurations.Users_Directory'Access,
		Log_File_Name => Configurations.Users_Log_File_Name'Access);
	
begin
	Debug.Hook (Configurations.Debug_Log_File_Name'Access, Now);
	Ada.Environment_Variables.Set ("TMPDIR", Configurations.Temporary_Directory);
	declare
		Lock : Web.Lock_Files.Lock_Type := Web.Lock_Files.Lock (Configurations.Lock_Name, Force => 60.0);
		-- HTTP Info
		Remote_Addr : String renames Web.Remote_Addr;
		Remote_Host : String renames Web.Remote_Host;
		Inputs : Web.Query_Strings renames Web.Get (Input);
		Query_Strings : Web.Query_Strings renames Web.Get_Query_Strings;
		Cookie : Web.Cookie := Web.Get_Cookie; -- variable
		Post : Boolean renames Web.Post;
		-- Values
		Renderer : Renderers.Renderer'Class renames Get_Renderer(Query_Strings);
		User_Id : String renames Renderers.Get_User_Id(Renderer, Query_Strings => Query_Strings, Cookie => Cookie);
		User_Password : String renames Renderers.Get_User_Password(Renderer, Query_Strings => Query_Strings, Cookie => Cookie);
		Village_Id : Tabula.Villages.Village_Id renames Renderers.Get_Village_Id(Renderer, Query_Strings);
		Cmd : String renames Web.Element (Inputs, "cmd");
		procedure Render_Reload_Page is
		begin
			Web.Header_See_Other (Output, Web.Request_URI);
			Web.Header_Content_Type (Output, Web.Text_HTML);
			Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
			Web.Header_Break (Output);
			Renderer.Refresh_Page (Output, URI => Web.Request_URI);
		end Render_Reload_Page;
	begin
		if Post and then (Remote_Host = "" or else Remote_Host = Remote_Addr)
			and then Remote_Addr /= "127.0.0.1" -- localhost
			and then Remote_Addr /= "::1" -- IPv6 localhost
			and then Remote_Addr /= "202.95.187.49" -- CATV
		then
			Web.Header_503 (Output);
			Web.Header_Break (Output);
		elsif Cmd = "logon" then
			declare
				New_User_Id : String renames Web.Element (Inputs, "id");
				New_User_Password : String renames Web.Element (Inputs, "password");
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
						Renderer.Set_User(Cookie,
							New_User_Id => New_User_Id, New_User_Password => New_User_Password);
						Web.Header_Content_Type (Output, Web.Text_HTML);
						Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
						Web.Header_Break (Output);
						Renderer.Message_Page(Output,
							Village_Id => Village_Id, Message => "ログオンしました。",
							User_Id => New_User_Id, User_Password => New_User_Password);
						Users.Lists.Update (Users_List,
							Id => New_User_Id,
							Remote_Addr => Remote_Addr, Remote_Host => Remote_Host,
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
							Remote_Addr => Remote_Addr, Remote_Host => Remote_Host,
							Now => Now,
							Info => User_Info);
					when others =>
						null;
				end case;
			end;
			Renderer.Set_User(Cookie,
				New_User_Id => "", New_User_Password => "");
			Web.Header_Content_Type (Output, Web.Text_HTML);
			Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
			Web.Header_Break (Output);
			Renderer.Message_Page(Output,
				Village_Id => Village_Id, Message => "ログオフしました。",
				User_Id => "", User_Password => "");
		elsif Cmd = "register" then
			declare
				New_User_Id : String renames Web.Element (Inputs, "id");
				New_User_Password : String renames Web.Element (Inputs, "password");
				New_User_Password_Retype : String renames Web.Element (Inputs, "password2");
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
						Renderer.Set_User(Cookie,
							New_User_Id => New_User_Id, New_User_Password => New_User_Password);
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
				Day_Duration : Duration;
			begin
				Users.Lists.Query (Users_List,
					Id => User_Id, Password => User_Password,
					Remote_Addr => Remote_Addr, Remote_Host => Remote_Host,
					Now => Now,
					Info => User_Info, State => User_State);
				case User_State is
					when Users.Lists.Valid =>
						if User_Id /= Users.Administrator
							and then Tabula.Villages.Lists.Created(
								User_Id,
								Tabula.Villages.Lists.Village_List (Renderers.Log.Load_Info'Access),
								Tabula.Villages.Invalid_Village_Id)
						then
							Web.Header_Content_Type (Output, Web.Text_HTML);
							Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
							Web.Header_Break (Output);
							Renderer.Message_Page(Output, Message => "同時に村をふたつ作成することはできません。",
								User_Id => User_Id, User_Password => User_Password);
						elsif Cmd = "news" and then (User_Info.Disallow_New_Village or else (
							Tabula.Villages.Lists.Short_Term_Village_Blocking
							and then User_Id /= Users.Administrator
							and then User_ID /= "she")) -- ハードコーディングですよ酷いコードですね
						then
							Web.Header_Content_Type (Output, Web.Text_HTML);
							Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
							Web.Header_Break (Output);
							Renderer.Message_Page(Output, Message => "えーと、しばらく短期は延期で。",
								User_Id => User_Id, User_Password => User_Password);
						else
							if Cmd = "news" then
								Day_Duration := Tabula.Default_Short_Day_Duration;
							else
								Day_Duration := Tabula.Default_Long_Day_Duration;
							end if;
							declare
								New_Village_Id : String renames Tabula.Villages.Lists.New_Village_Id;
								Village_Name : String renames Web.Element (Inputs, "name");
								Village : Villages.Village_Type := (
									Name => +Village_Name,
									By => +User_Id,
									State => Tabula.Villages.Prologue,
									Today => 0,
									Time => Tabula.Villages.Daytime,
									Dawn => Now,
									Day_Duration => Day_Duration,
									Night_Duration => Default_Night_Duration,
									Execution            => Villages.Initial_Execution,
									Teaming              => Villages.Initial_Teaming,
									Monster_Side         => Villages.Initial_Monster_Side,
									Attack               => Villages.Initial_Attack,
									Servant_Knowing      => Villages.Initial_Servant_Knowing,
									Daytime_Preview      => Villages.Initial_Daytime_Preview,
									Doctor_Infected      => Villages.Initial_Doctor_Infected,
									Hunter_Silver_Bullet => Villages.Initial_Hunter_Silver_Bullet,
									Unfortunate          => Villages.Initial_Unfortunate,
									Appearance => (others => Villages.Random),
									Dummy_Role => Villages.Inhabitant,
									People => Empty_Vector,
									Escaped_People => Empty_Vector,
									Messages => Villages.Messages.Empty_Vector);
							begin
								if Village.Name = "" then
									Web.Header_Content_Type (Output, Web.Text_HTML);
									Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
									Web.Header_Break (Output);
									Renderer.Message_Page(Output,
										Message => "村名を入力してください。",
										User_Id => User_Id, User_Password => User_Password);
								else
									begin
										Villages.Save(New_Village_Id, Village);
										Web.Header_Content_Type (Output, Web.Text_HTML);
										Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
										Web.Header_Break (Output);
										Renderer.Message_Page(Output,
											Message => "新たな村「" & Village_Name & "」を作成しました。",
											User_Id => User_Id, User_Password => User_Password);
										Tabula.Villages.Lists.Update_Village_List (
											Remake_All => False,
											Load_Info => Renderers.Log.Load_Info'Access,
											Create_Log => Renderers.Log.Create_Log'Access);
										Users.Lists.Update (Users_List,
											Id => User_Id,
											Remote_Addr => Remote_Addr, Remote_Host => Remote_Host,
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
					Tabula.Villages.Lists.Update_Village_List (
						Remake_All => True,
						Load_Info => Renderers.Log.Load_Info'Access,
						Create_Log => Renderers.Log.Create_Log'Access);
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
				elsif Renderer.Is_User_Page(Query_Strings => Query_Strings, Cookie => Cookie) then
					declare
						User_State : Users.Lists.User_State;
						User_Info : Users.User_Info;
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
							Renderer.User_Page(Output,
								Tabula.Villages.Lists.Village_List (Renderers.Log.Load_Info'Access),
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
				elsif Web.Element (Query_Strings, "users") = "all" then
					Web.Header_Content_Type (Output, Web.Text_HTML);
					Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
					Web.Header_Break (Output);
					Renderers.Users_Page (Renderer, Output,
						Tabula.Villages.Lists.Village_List (Renderers.Log.Load_Info'Access),
						User_List => Users.Lists.All_Users (Users_List),
						User_Id => User_Id,
						User_Password => User_Password);
				else
					Web.Header_Content_Type (Output, Web.Text_HTML);
					Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
					Web.Header_Break (Output);
					Renderer.Index_Page(Output,
						Tabula.Villages.Lists.Village_List (Renderers.Log.Load_Info'Access),
						Users.Lists.Muramura_Count (Users_List, Now),
						User_Id => User_Id,
						User_Password => User_Password);
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
				elsif not Tabula.Villages.Lists.Exists (Village_Id) then
					Web.Header_Content_Type (Output, Web.Text_HTML);
					Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
					Web.Header_Break (Output);
					Renderer.Error_Page(Output, "存在しない村が指定されました。");
				else
					declare
						Village : aliased Villages.Village_Type;
					begin
						Villages.Load(Village_Id, Village);
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
										Villages.Save(Village_Id, Village);
									end if;
									if List_Changed then
										Tabula.Villages.Lists.Update_Village_List (
											Remake_All => False,
											Load_Info => Renderers.Log.Load_Info'Access,
											Create_Log => Renderers.Log.Create_Log'Access);
									end if;
								end;
								-- 村レンダリング
								declare
									Day : Natural;
									First, Last : Integer;
								begin
									Web.Header_Content_Type (Output, Web.Text_HTML);
									Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
									Web.Header_Break (Output);
									Renderer.Get_Day(Village, Query_Strings, Day);
									Renderer.Get_Range(Village, Day, Query_Strings, First, Last);
									Renderers.Village_Page(
										Renderer,
										Output,
										Village_Id,
										Village'Access,
										Day => Day,
										First => First,
										Last => Last,
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
									if Village.State = Tabula.Villages.Opened
										and then Village.People.Constant_Reference(Player).Element.Records.Constant_Reference(Village.Today).Element.State = Villages.Died
									then
										Web.Header_Content_Type (Output, Web.Text_HTML);
										Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
										Web.Header_Break (Output);
										Renderers.Message_Page(Renderer, Output, Village_Id, Village'Access, "あなたは死にました。", User_Id, User_Password);
										return False;
									elsif Village.Time /= Tabula.Villages.Daytime then
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
							begin
								if User_State /= Users.Lists.Valid then
									Web.Header_Content_Type (Output, Web.Text_HTML);
									Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
									Web.Header_Break (Output);
									Renderer.Error_Page(Output, "正常にログオンしてください。");
								elsif Cmd = "join" then
									declare
										Village_List : Tabula.Villages.Lists.Village_Lists.Vector
											renames Tabula.Villages.Lists.Village_List (Renderers.Log.Load_Info'Access);
									begin
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
										elsif Tabula.Villages.Lists.Created(User_Id, Village_List, Village_Id) then
											Web.Header_Content_Type (Output, Web.Text_HTML);
											Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
											Web.Header_Break (Output);
											Renderer.Message_Page(Output, Village_Id, Village'Access, "自分の作成した村に入ってください。", User_Id, User_Password);
										elsif Village.Day_Duration >= 24 * 60 * 60.0
											and then Tabula.Villages.Lists.Joined(User_Id, Village_List, Long_Only => True)
										then
											Web.Header_Content_Type (Output, Web.Text_HTML);
											Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
											Web.Header_Break (Output);
											Renderer.Message_Page(Output, Village_Id, Village'Access, "既に他の村に参加しています。", User_Id, User_Password);
										else
											declare
												Work_Num : Integer := Natural'Value(Web.Element(Inputs, "work"));
												Name_Num : constant Natural := Natural'Value(Web.Element(Inputs, "name"));
												Request : constant Villages.Requested_Role := Villages.Requested_Role'Value(Web.Element(Inputs, "request"));
												Cast : Casts.Cast_Collection := Casts.Load (Configurations.Cast_File_Name);
											begin
												Villages.Exclude_Taken (Cast, Village);
												declare
													Person_Template : Casts.Person renames Cast.People.Constant_Reference (Name_Num).Element.all;
												begin
													if Work_Num < 0 then
														Searching_Established_Work : for I in Cast.Works.First_Index .. Cast.Works.Last_Index loop
															if Person_Template.Work = Cast.Works.Constant_Reference(I).Element.Name then
																Work_Num := I;
																exit Searching_Established_Work;
															end if;
														end loop Searching_Established_Work;
													end if;
													if Work_Num < 0 then
														Web.Header_Content_Type (Output, Web.Text_HTML);
														Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
														Web.Header_Break (Output);
														Renderer.Message_Page(Output, Village_Id, Village'Access,
															"申し訳ありませんが既定の肩書き(" & (+Person_Template.Work) & ")は既に取られています。",
															User_Id, User_Password);
													else
														declare
															Selected_Work : access constant Casts.Work := Cast.Works.Constant_Reference(Work_Num).Element;
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
															elsif Villages.Already_Joined_Another_Sex(Village, User_Id, Person_Template.Sex) then
																Web.Header_Content_Type (Output, Web.Text_HTML);
																Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
																Web.Header_Break (Output);
																Renderer.Message_Page(Output, Village_Id, Village'Access, "以前にエントリされたときと性別が異なります。", User_Id, User_Password);
															else
																Append(Village.People, Villages.Person_Type'(
																	Name => Person_Template.Name,
																	Image => Person_Template.Image,
																	Sex => Person_Template.Sex,
																	Group => Person_Template.Group,
																	Work => Selected_Work.Name,
																	Request => Request,
																	Ignore_Request => User_Info.Ignore_Request,
																	Role => Villages.Inhabitant,
																	Id => +User_Id,
																	Commited => False,
																	Records => To_Vector (Villages.Default_Person_Record, Length => 1)));
																Add (Village, Villages.Join, Subject => Village.People.Last_Index);
																Villages.Save(Village_Id, Village);
																Tabula.Villages.Lists.Update_Village_List (
																	Remake_All => False,
																	Load_Info => Renderers.Log.Load_Info'Access,
																	Create_Log => Renderers.Log.Create_Log'Access);
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
									end;
								elsif Cmd = "narration" then
									if User_Id /= Tabula.Users.Administrator then
										Web.Header_Content_Type (Output, Web.Text_HTML);
										Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
										Web.Header_Break (Output);
										Renderer.Error_Page(Output, "administratorのみに許された操作です。");
									else
										declare
											Text : String renames Renderers.Get_Text(Renderer, Inputs);
										begin
											Add(Village, Villages.Narration, Text => Text);
											Villages.Save(Village_Id, Village);
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
											Target : constant Integer := Integer'Value(Web.Element(Inputs, "target"));
											Removed_Id : constant String := +Village.People.Constant_Reference(Target).Element.Id;
										begin
											Villages.Escape(Village, Target, Now);
											Villages.Save(Village_Id, Village);
											Tabula.Villages.Lists.Update_Village_List (
												Remake_All => False,
												Load_Info => Renderers.Log.Load_Info'Access,
												Create_Log => Renderers.Log.Create_Log'Access);
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
									if Village.Time = Tabula.Villages.Night then
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
												Villages.Save(Village_Id, Village);
												if List_Changed then
													Tabula.Villages.Lists.Update_Village_List (
														Remake_All => False,
														Load_Info => Renderers.Log.Load_Info'Access,
														Create_Log => Renderers.Log.Create_Log'Access);
												end if;
											end;
										end if;
										Render_Reload_Page;
									end if;
								elsif Cmd = "rollback" then
									if Village.People.Constant_Reference(Player).Element.Commited then
										Village.People.Reference(Player).Element.Commited := False;
										Villages.Save(Village_Id, Village);
									end if;
									Render_Reload_Page;
								elsif Cmd = "escape" then
									if Village.State /= Tabula.Villages.Prologue then
										Web.Header_Content_Type (Output, Web.Text_HTML);
										Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
										Web.Header_Break (Output);
										Renderer.Error_Page(Output, "村から出られるのはプロローグのみです。");
									else
										declare
											OK : Boolean := False;
										begin
											begin
												declare
													X : constant Integer := Integer'Value(Web.Element(Inputs, "x"));
													Y : constant Integer := Integer'Value(Web.Element(Inputs, "y"));
													Z : constant Integer := Integer'Value(Web.Element(Inputs, "z"));
												begin
													OK := X + Y = Z;
												end;
											exception
												when Constraint_Error =>
													Web.Header_Content_Type (Output, Web.Text_HTML);
													Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
													Web.Header_Break (Output);
													Renderers.Message_Page(Renderer, Output, Village_Id, Village'Access, "答えを入力してください。", User_Id, User_Password);
													goto Exit_Answer;
											end;
											if OK then
												Villages.Escape(Village, Player, Now);
												Villages.Save(Village_Id, Village);
												Tabula.Villages.Lists.Update_Village_List (
													Remake_All => False,
													Load_Info => Renderers.Log.Load_Info'Access,
													Create_Log => Renderers.Log.Create_Log'Access);
												Web.Header_Content_Type (Output, Web.Text_HTML);
												Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
												Web.Header_Break (Output);
												Renderers.Message_Page(Renderer, Output, Village_Id, Village'Access, "旅に出ました。", User_Id, User_Password);
											else
												Web.Header_Content_Type (Output, Web.Text_HTML);
												Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
												Web.Header_Break (Output);
												Renderers.Message_Page(Renderer, Output, Village_Id, Village'Access, "答えが違います。", User_Id, User_Password);
											end if;
											<<Exit_Answer>> null;
										end;
									end if;
								elsif Cmd = "action" then
									declare
										Action : String renames Web.Element(Inputs, "action");
										Said : Villages.Message_Counts renames Villages.Count_Messages(Village, Village.Today);
										Target : constant Integer := Integer'Value(Web.Element(Inputs, "target"));
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
												Village.People.Reference(Target).Element.Commited := False;
												Add(Village, Villages.Action_Wake, Subject => Player, Target => Target);
												Villages.Save(Village_Id, Village);
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
												Add(Village, Villages.Action_Encourage, Subject => Player, Target => Target);
												Villages.Save(Village_Id, Village);
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
											elsif Village.Time = Tabula.Villages.Night then
												Web.Header_Content_Type (Output, Web.Text_HTML);
												Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
												Web.Header_Break (Output);
												Renderers.Message_Page(Renderer, Output, Village_Id, Village'Access, "夜は直接会話できます。", User_Id, User_Password);
											elsif Villages.Unfortunate(Village) then
												Add(Village, Villages.Action_Vampire_Gaze_Blocked, Subject => Player, Target => Target);
												Villages.Save(Village_Id, Village);
												Render_Reload_Page;
											else
												Add(Village, Villages.Action_Vampire_Gaze, Subject => Player, Target => Target);
												Villages.Save(Village_Id, Village);
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
											Text : String renames Renderers.Get_Text(Renderer, Inputs);
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
											Text : String renames Renderers.Get_Text(Renderer, Inputs);
										begin
											if Text'Length > 0 then
												Add(Village, Villages.Speech, Subject => Player, Text => Text);
												Villages.Save(Village_Id, Village);
											end if;
										end;
										Render_Reload_Page;
									end if;
								elsif Cmd = "reedit" then
									declare
										Kind : Villages.Message_Kind renames Villages.Message_Kind'Value (Web.Element (Inputs, "kind"));
									begin
										case Kind is
											when Villages.Speech =>
												if Speech_Check then
													declare
														Text : String renames Renderers.Get_Text(Renderer, Inputs);
														Day : Natural;
														First, Last : Integer;
													begin
														Web.Header_Content_Type (Output, Web.Text_HTML);
														Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
														Web.Header_Break (Output);
														Renderer.Get_Day(Village, Query_Strings, Day);
														Renderer.Get_Range(Village, Day, Query_Strings, First, Last);
														Renderers.Village_Page(
															Renderer,
															Output,
															Village_Id,
															Village'Access,
															Day => Day,
															First => First,
															Last => Last,
															Editing_Text => Text,
															User_Id => User_Id,
															User_Password => User_Password);
													end;
												end if;
											when others =>
												-- 通常発言以外の再編集は未実装……
												Render_Reload_Page;
										end case;
									end;
								elsif Cmd = "monologue" then
									if Village.State = Tabula.Villages.Epilogue then
										Web.Header_Content_Type (Output, Web.Text_HTML);
										Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
										Web.Header_Break (Output);
										Renderer.Error_Page(Output, "エピローグでは独白は喋れません。");
									else
										declare
											Text : String renames Renderers.Get_Text(Renderer, Inputs);
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
													Add(Village, Villages.Monologue, Subject => Player, Text => Text);
													Villages.Save(Village_Id, Village);
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
											Text : String renames Renderers.Get_Text(Renderer, Inputs);
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
													Add(Village, Villages.Ghost, Subject => Player, Text => Text);
													Villages.Save(Village_Id, Village);
												end if;
												Render_Reload_Page;
											end if;
										end;
									end if;
								elsif Cmd = "note" then
									declare
										Text : String renames Renderers.Get_Text(Renderer, Inputs);
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
												if Village.Time = Tabula.Villages.Night
													and then Village.People.Constant_Reference(Player).Element.Role in Villages.Vampire_Role
													and then Village.People.Constant_Reference(Player).Element.Records.Constant_Reference(Village.Today).Element.State /= Villages.Died
												then
													Villages.Night_Talk (Village, Player, Text, Now);
												else
													Village.People.Reference(Player).Element.Records.Reference(Village.Today).Element.Note := +Text;
												end if;
												Villages.Save(Village_Id, Village);
											end if;
											Render_Reload_Page;
										end if;
									end;
								elsif Cmd = "vote" then
									if Village.Time = Tabula.Villages.Night then
										Web.Header_Content_Type (Output, Web.Text_HTML);
										Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
										Web.Header_Break (Output);
										Renderer.Message_Page(Output, Village_Id, Village'Access, "今は投票できない時間帯です。", User_Id, User_Password);
									else
										declare
											Target : constant Integer := Integer'Value(Web.Element(Inputs, "target"));
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
													Villages.Save(Village_Id, Village);
												end if;
												Render_Reload_Page;
											end if;
										end;
									end if;
								elsif Cmd = "target" then
									declare
										Target : constant Integer := Integer'Value(Web.Element(Inputs, "target"));
										Special : constant Boolean := Web.Checkbox_Value(Web.Element(Inputs, "special"));
										Target_Day : Natural := Village.Today;
										Special_Used : Boolean := False;
									begin
										if Village.Time = Tabula.Villages.Night then
											Target_Day := Target_Day - 1;
										end if;
										for I in 0 .. Target_Day - 1 loop
											if Village.People.Constant_Reference(Player).Element.Records.Constant_Reference(I).Element.Special then
												Special_Used := True;
											end if;
										end loop;
										if Special_Used and Special then
											Web.Header_Content_Type (Output, Web.Text_HTML);
											Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
											Web.Header_Break (Output);
											Renderer.Error_Page(Output, "銀の弾丸は一発限りです。");
										elsif Village.Daytime_Preview /= Villages.None
											and then (Village.People.Constant_Reference(Player).Element.Role = Villages.Detective
											or else Village.People.Constant_Reference(Player).Element.Role = Villages.Doctor)
										then
											if Village.Time = Tabula.Villages.Night then
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
												Village.People.Reference(Player).Element.Records.Reference(Target_Day).Element.Target := Target;
												Village.People.Reference(Player).Element.Records.Reference(Target_Day).Element.Special := Special;
												Villages.Save(Village_Id, Village);
											end if;
											Render_Reload_Page;
										end if;
									end;
								elsif Cmd = "target2" then
									if Village.Time = Tabula.Villages.Night then
										Web.Header_Content_Type (Output, Web.Text_HTML);
										Web.Header_Cookie (Output, Cookie, Now + Cookie_Duration);
										Web.Header_Break (Output);
										Renderer.Message_Page(Output, Village_Id, Village'Access, "医者と探偵は、夜に能力を使えません。", User_Id, User_Password);
									else
										declare
											Target : constant Integer := Integer'Value(Web.Element(Inputs, "target"));
										begin
											case Village.People.Constant_Reference (Player).Element.Role is
												when Villages.Doctor =>
													if Village.People.Constant_Reference(Target).Element.Role = Villages.Gremlin then
														Add(Village, Villages.Doctor_Found_Gremlin_Preview, Subject => Player, Target => Target);
													elsif Village.People.Constant_Reference(Target).Element.Records.Constant_Reference(Village.Today).Element.State = Villages.Infected then
														declare
															Result : Villages.Message_Kind := Villages.Doctor_Cure_Preview;
														begin
															if Village.Doctor_Infected = Villages.Find_Infection
																and then Village.People.Constant_Reference(Player).Element.Records.Constant_Reference(Village.Today).Element.State = Villages.Infected
															then
																Result := Villages.Doctor_Found_Infection_Preview;
															end if;
															Add(Village, Result, Subject => Player, Target => Target);
														end;
													else
														Add(Village, Villages.Doctor_Failed_Preview, Subject => Player, Target => Target);
													end if;
													Village.People.Reference(Player).Element.Records.Reference(Village.Today).Element.Target := Target;
													Villages.Save(Village_Id, Village);
													Render_Reload_Page;
												when Villages.Detective =>
													Add (Village, Villages.Detective_Survey_Preview,
														Subject => Player, Target => Target,
														Text => +Village.People.Constant_Reference (Target).Element.Records.Constant_Reference (Village.Today).Element.Note);
													Village.People.Reference (Player).Element.Records.Reference (Village.Today).Element.Target := Target;
													Villages.Save(Village_Id, Village);
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
										declare
											procedure Process (Item : in Tabula.Villages.Root_Option_Item'Class) is
											begin
												Tabula.Villages.Change (Village'Access, Item, Web.Element (Inputs, Item.Name));
											end Process;
										begin
											Vampires.Villages.Iterate (Village'Access, Process'Access);
										end;
										Villages.Save(Village_Id, Village);
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
		String'Write(Output, "White fog. Wait 1 minute!" & Ascii.LF);
	when E : others =>
		Web.Header_Content_Type (Output, Web.Text_Plain);
		Web.Header_Break(Output);
		String'Write(Output, Ada.Exceptions.Exception_Information(E));
		Character'Write(Output, Ascii.LF);
end Tabula.Vampires.Main;
