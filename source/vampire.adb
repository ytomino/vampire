-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Calendar;
with Ada.Environment_Variables;
with Ada.Exceptions;
with Ada.Finalization;
with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO.Text_Streams;
with Ase.Directories.Lock_Files;
with Ase.Numerics.MT19937;
with Ase.Streams.Standard_Types;
with Ase.Web;
with Tabula.Calendar;
with Tabula.Configurations.Templates;
with Tabula.Renderers.Error_Page;
with Tabula.Renderers.Message_Page;
with Tabula.Renderers.Rule;
with Tabula.Renderers.Simple;
with Tabula.Users.Managing;
with Tabula.Villages.Advance;
with Tabula.Villages.Casts.Load;
with Tabula.Villages.Lists.Managing;
with Tabula.Villages.Load;
with Tabula.Villages.Save;
procedure Vampire is
	
	package MT19937 renames Ase.Numerics.MT19937;
	use Ase.Streams.Standard_Types;
	use Tabula;
	use Villages.Messages;
	use Villages.Person_Arrays;
	use type Ada.Calendar.Time;
	use type Ada.Strings.Unbounded.Unbounded_String;
	use type Users.Managing.Check_Result;
	use type Villages.Attack_Mode;
	use type Villages.Doctor_Infected_Mode;
	use type Villages.Daytime_Preview_Mode;
	use type Villages.Village_State;
	use type Villages.Village_Time;
	use type Villages.Person_Role;
	use type Villages.Person_Sex;
	use type Villages.Person_State;
	use type Villages.Person_Array_Access;
	use type Villages.Message_Kind;
	use type Villages.Message;
	use type Villages.Casts.Work;
	function "+" (S : Ada.Strings.Unbounded.Unbounded_String) return String renames Ada.Strings.Unbounded.To_String;
	function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String renames Ada.Strings.Unbounded.To_Unbounded_String;
	
	Now : constant Ada.Calendar.Time := Ada.Calendar.Clock;
	
	Generator : MT19937.Generator;
	
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
		if Is_Empty(Village.Messages) or else Element(Last(Village.Messages)) /= New_Item then
			Append(Village.Messages, New_Item);
		end if;
	end Add;
	
	function Get_Renderer(Query_Strings : in Ase.Web.Query_Strings) return Renderers.Renderer'Class is
	begin
		if Ase.Web.Element(Query_Strings, "b") = "k" then
			return Renderers.Simple.Renderer'(Configuration => Configurations.Templates.Simple_Configuration);
		else
			return Renderers.Renderer'(Configuration => Configurations.Templates.Configuration);
		end if;
	end Get_Renderer;
	
	Input : Ada.Text_IO.Text_Streams.Stream_Access 
		renames Ada.Text_IO.Text_Streams.Stream(Ada.Text_IO.Standard_Input);
	Output : Ada.Text_IO.Text_Streams.Stream_Access 
		renames Ada.Text_IO.Text_Streams.Stream(Ada.Text_IO.Standard_Output);
	
	Lock : Ase.Directories.Lock_Files.Lock_Type(Configurations.Lock_Name'Access);
begin
	Ase.Directories.Lock_Files.Lock(Lock, Force => 60.0);
	MT19937.Reset(Generator);
	declare
		-- HTTP Info
		Remote_Addr : String renames Ada.Environment_Variables.Value(Ase.Web.Remote_Addr_Variable);
		Remote_Host : String renames Ada.Environment_Variables.Value(Ase.Web.Remote_Host_Variable);
		Inputs : Ase.Web.Query_Strings renames Ase.Web.Get(Input);
		Query_Strings : Ase.Web.Query_Strings renames Ase.Web.Get_Query_Strings;
		Cookie : Ase.Web.Cookie := Ase.Web.Get_Cookie; -- variable
		Post : Boolean renames Ase.Web.Post;
		-- Values
		Renderer : Renderers.Renderer'Class renames Get_Renderer(Query_Strings);
		User_Id : String renames Renderers.Get_User_Id(Renderer, Query_Strings => Query_Strings, Cookie => Cookie);
		User_Password : String renames Renderers.Get_User_Password(Renderer, Query_Strings => Query_Strings, Cookie => Cookie);
		Village_Id : Villages.Lists.Village_Id renames Renderers.Get_Village_Id(Renderer, Query_Strings);
		Cmd : String renames Ase.Web.Element(Inputs, "cmd");
		procedure Render_Reload_Page is
		begin
			Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration, Ase.Web.Request_URI);
			Ase.Web.Header_Break(Output);
			Renderer.Refresh_Page(Output, URI => Ase.Web.Request_URI);
		end Render_Reload_Page;
	begin
		if Post and then (Remote_Host = "" or else Remote_Host = Remote_Addr) 
			and then Remote_Addr /= "127.0.0.1" -- localhost
			and then Remote_Addr /= "202.95.187.49" -- CATV
		then
			Ase.Web.Header_503(Output);
			Ase.Web.Header_Break(Output);
		elsif Cmd = "logon" then
			declare
				New_User_Id : String renames Ase.Web.Element(Inputs, "id");
				New_User_Password : String renames Ase.Web.Element(Inputs, "password");
				User_State : Users.Managing.Check_Result;
				User_Info : Users.User_Info;
			begin
				Users.Managing.Check(Id => New_User_Id, Password => New_User_Password, 
					Remote_Addr => Remote_Addr, Remote_Host => Remote_Host, Now => Now,
					Result => User_State, User_Info => User_Info);
				case User_State is
					when Users.Managing.Log_Off =>
						Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
						Ase.Web.Header_Break(Output);
						Renderer.Message_Page(Output,
							Village_Id => Village_Id, Message => "IDを入力してください。",
							User_Id => "", User_Password => "");
					when Users.Managing.Unknown =>
						if Users.Valid_Id_String(New_User_Id) then
							Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
							Ase.Web.Header_Break(Output);
							Renderer.Register_Page(Output,
								Village_Id => Village_Id,
								New_User_Id => New_User_Id, New_User_Password => New_User_Password);
						else
							Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
							Ase.Web.Header_Break(Output);
							Renderer.Message_Page(Output, 
								Village_Id => Village_Id, Message => "変な文字は使わないでください。", 
								User_Id => "", User_Password => "");
						end if;
					when Users.Managing.Invalid =>
						Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
						Ase.Web.Header_Break(Output);
						Renderer.Message_Page(Output, 
							Village_Id => Village_Id, Message => "パスワードが異なります。", 
							User_Id => "", User_Password => "");
					when Users.Managing.Valid =>
						Renderer.Set_User(Cookie, 
							New_User_Id => New_User_Id, New_User_Password => New_User_Password);
						Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
						Ase.Web.Header_Break(Output);
						Renderer.Message_Page(Output,
							Village_Id => Village_Id, Message => "ログオンしました。", 
							User_Id => New_User_Id, User_Password => New_User_Password);
						Users.Managing.Update(Id => New_User_Id,
							Remote_Addr => Remote_Addr, Remote_Host => Remote_Host, Time => Now,
							User_Info => User_Info);
				end case;
			end;
		elsif Cmd = "logoff" then
			declare
				User_State : Users.Managing.Check_Result;
				User_Info : Users.User_Info;
			begin
				Users.Managing.Check(Id => User_Id, Password => User_Password, 
					Remote_Addr => Remote_Addr, Remote_Host => Remote_Host, Now => Now,
					Result => User_State, User_Info => User_Info);
				if User_State = Users.Managing.Valid then
					Users.Managing.Update(Id => User_Id,
						Remote_Addr => Remote_Addr, Remote_Host => Remote_Host, Time => Now,
						User_Info => User_Info);
				end if;
			end;
			Renderer.Set_User(Cookie, 
				New_User_Id => "", New_User_Password => "");
			Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
			Ase.Web.Header_Break(Output);
			Renderer.Message_Page(Output,
				Village_Id => Village_Id, Message => "ログオフしました。", 
				User_Id => "", User_Password => "");
		elsif Cmd = "register" then
			declare
				New_User_Id : String renames Ase.Web.Element(Inputs, "id");
				New_User_Password : String renames Ase.Web.Element(Inputs, "password");
				New_User_Password_Retype : String renames Ase.Web.Element(Inputs, "password2");
				Registered : Boolean;
			begin
				if New_User_Password /= New_User_Password_Retype then
					Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
					Ase.Web.Header_Break(Output);
					Renderer.Message_Page(Output, 
						Village_Id => Village_Id, Message => "再入力されたパスワードが異なります。", 
						User_Id => "", User_Password => "");
				else
					Users.Managing.New_User(Id => New_User_Id, Password => New_User_Password,
						Remote_Addr => Remote_Addr, Remote_Host => Remote_Host, Now => Now,
						Result => Registered);
					if Registered then
						Renderer.Set_User(Cookie, 
							New_User_Id => New_User_Id, New_User_Password => New_User_Password);
						Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
						Ase.Web.Header_Break(Output);
						Renderer.Message_Page(Output,
							Village_Id => Village_Id, Message => "登録しました。", 
							User_Id => New_User_Id, User_Password => New_User_Password);
					else
						Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
						Ase.Web.Header_Break(Output);
						Renderer.Message_Page(Output, 
							Village_Id => Village_Id, Message => "登録に失敗しました。", 
							User_Id => "", User_Password => "");
					end if;
				end if;
			end;
		elsif Cmd = "newl" or else Cmd = "news" then -- 村作成
			declare
				User_State : Users.Managing.Check_Result;
				User_Info : Users.User_Info;
				Day_Duration : Duration;
			begin
				Users.Managing.Check(Id => User_Id, Password => User_Password, 
					Remote_Addr => Remote_Addr, Remote_Host => Remote_Host, Now => Now,
					Result => User_State, User_Info => User_Info);
				if User_State = Users.Managing.Valid then
					if User_Id /= Users.Administrator 
						and then Villages.Lists.Created(User_Id, Villages.Lists.Managing.Village_List, Villages.Lists.Invalid_Village_Id)
					then
						Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
						Ase.Web.Header_Break(Output);
						Renderer.Message_Page(Output, Message => "同時に村をふたつ作成することはできません。",
							User_Id => User_Id, User_Password => User_Password);
					elsif Cmd = "news" and then (User_Info.Disallow_New_Village or else (
						Villages.Lists.Managing.Short_Term_Village_Blocking
						and then User_Id /= Users.Administrator
						and then User_ID /= "she")) -- ハードコーディングですよ酷いコードですね
					then
						Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
						Ase.Web.Header_Break(Output);
						Renderer.Message_Page(Output, Message => "えーと、しばらく短期は延期で。",
							User_Id => User_Id, User_Password => User_Password);
					else
						if Cmd = "news" then
							Day_Duration := Tabula.Default_Short_Day_Duration;
						else
							Day_Duration := Tabula.Default_Long_Day_Duration;
						end if;
						declare
							New_Village_Id : String renames Tabula.Villages.Lists.Managing.New_Village_Id;
							Village_Name : String renames Ase.Web.Element(Inputs, "name");
							Village : Tabula.Villages.Village_Type := (Ada.Finalization.Limited_Controlled with
								Name => +Village_Name,
								By => +User_Id,
								State => Villages.Prologue,
								Today => 0,
								Time => Villages.Daytime,
								Dawn => Now,
								Day_Duration => Day_Duration,
								Night_Duration => Default_Night_Duration,
								Victim_Existing      => Villages.Initial_Victim_Existing, Victim_Role => Villages.Inhabitant,
								Teaming              => Villages.Initial_Teaming,
								Monster_Side         => Villages.Initial_Monster_Side,
								Attack               => Villages.Initial_Attack,
								Servant_Knowing      => Villages.Initial_Servant_Knowing,
								Daytime_Preview      => Villages.Initial_Daytime_Preview,
								Doctor_Infected      => Villages.Initial_Doctor_Infected,
								Hunter_Silver_Bullet => Villages.Initial_Hunter_Silver_Bullet,
								Unfortunate          => Villages.Initial_Unfortunate,
								Appearance => (others => Villages.Random),
								People => null,
								Escaped_People => null,
								Messages => Villages.Messages.Empty_Vector);
						begin
							if Village.Name = "" then
								Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
								Ase.Web.Header_Break(Output);
								Renderer.Message_Page(Output,
									Message => "村名を入力してください。", 
									User_Id => User_Id, User_Password => User_Password);
							else
								begin
									Villages.Save(New_Village_Id, Village);
									Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
									Ase.Web.Header_Break(Output);
									Renderer.Message_Page(Output,
										Message => "新たな村「" & Village_Name & "」を作成しました。", 
										User_Id => User_Id, User_Password => User_Password);
									Tabula.Villages.Lists.Managing.Refresh_Village_List;
									Users.Managing.Update(Id => User_Id,
										Remote_Addr => Remote_Addr, Remote_Host => Remote_Host, Time => Now,
										User_Info => User_Info);
								exception
									when Ada.IO_Exceptions.Name_Error =>
										Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
										Ase.Web.Header_Break(Output);
										Renderer.Message_Page(Output,
											Message => "作成に失敗しました。", 
											User_Id => User_Id, User_Password => User_Password);
								end;
							end if;
						end;
					end if;
				else
					Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
					Ase.Web.Header_Break(Output);
					Renderer.Error_Page(Output, "正常にログオンしてください。");
				end if;
			end;
		elsif Cmd = "remakelog" then
			declare
				User_State : Users.Managing.Check_Result;
				User_Info : Users.User_Info;
			begin
				Users.Managing.Check(Id => User_Id, Password => User_Password, 
					Remote_Addr => Remote_Addr, Remote_Host => Remote_Host, Now => Now,
					Result => User_State, User_Info => User_Info);
				if User_State = Users.Managing.Valid and then User_Id = Tabula.Users.Administrator then
					Villages.Lists.Managing.Clear_Village_List;
					Render_Reload_Page;
				else
					Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
					Ase.Web.Header_Break(Output);
					Renderer.Error_Page(Output, "administratorのみに許された操作です。");
				end if;
			end;
		elsif Village_Id = Villages.Lists.Invalid_Village_Id then
			if Cmd = "" then
				if Post then
					Render_Reload_Page;
				elsif Renderer.Is_User_Page(Query_Strings => Query_Strings, Cookie => Cookie) then
					declare
						User_State : Users.Managing.Check_Result;
						User_Info : Users.User_Info;
					begin
						Users.Managing.Check(Id => User_Id, Password => User_Password, 
							Remote_Addr => Remote_Addr, Remote_Host => Remote_Host, Now => Now,
							Result => User_State, User_Info => User_Info);
						if User_State = Users.Managing.Valid and then User_Id /= Users.Administrator then
							Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
							Ase.Web.Header_Break(Output);
							Renderer.User_Page(Output,
								Tabula.Villages.Lists.Managing.Village_List,
								User_Id => User_Id, 
								User_Password => User_Password,
								User_Info => User_Info);
						else
							Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
							Ase.Web.Header_Break(Output);
							Renderer.Error_Page(Output, "正常にログオンしないとユーザーページは表示できません。");
						end if;
					end;
				else
					Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
					Ase.Web.Header_Break(Output);
					Renderer.Index_Page(Output,
						Tabula.Villages.Lists.Managing.Village_List,
						Users.Managing.Muramura_Count(Now),
						User_Id => User_Id, 
						User_Password => User_Password);
				end if;
			else
				Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
				Ase.Web.Header_Break(Output);
				Renderer.Error_Page(Output, "不正なコマンド(" & Cmd & ")が送られました。");
			end if;
		else
			declare
				User_State : Users.Managing.Check_Result;
				User_Info : Users.User_Info;
			begin
				Users.Managing.Check(Id => User_Id, Password => User_Password, 
					Remote_Addr => Remote_Addr, Remote_Host => Remote_Host, Now => Now,
					Result => User_State, User_Info => User_Info);
				if User_State = Users.Managing.Invalid then
					Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
					Ase.Web.Header_Break(Output);
					Renderer.Error_Page(Output, "パスワードが不正です。");
				elsif not Villages.Lists.Managing.Exists(Village_Id) then
					Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
					Ase.Web.Header_Break(Output);
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
									Villages.Advance(Village, Now, Generator,
										Changed => Changed, List_Changed => List_Changed);
									if Changed then
										Villages.Save(Village_Id, Village);
									end if;
									if List_Changed then
										Tabula.Villages.Lists.Managing.Refresh_Village_List;
									end if;
								end;
								-- 村レンダリング
								declare
									Day : Natural;
									First, Last : Integer;
								begin
									Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
									Ase.Web.Header_Break(Output);
									Renderer.Get_Day(Village, Query_Strings, Day);
									Renderer.Get_Range(Village, Day, Query_Strings, First, Last);
									Renderer.Village_Page(Output,
										Village_Id, 
										Village, 
										Day => Day, 
										First => First,
										Last => Last,
										User_Id => User_Id,
										User_Password => User_Password);
								end;
							end if;
						elsif Village.State = Villages.Closed then
							Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
							Ase.Web.Header_Break(Output);
							Renderer.Error_Page(Output, "終了した村にコマンドが送られました。");
						else
							declare
								Player : Integer;
							begin
								Player := Villages.Joined(Village, User_Id);
								if User_State /= Users.Managing.Valid then
									Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
									Ase.Web.Header_Break(Output);
									Renderer.Error_Page(Output, "正常にログオンしてください。");
								elsif Cmd = "join" then
									declare
										Village_List : Villages.Lists.Village_Lists.Vector renames Tabula.Villages.Lists.Managing.Village_List;
									begin
										if Player >= 0 then
											Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
											Ase.Web.Header_Break(Output);
											Renderer.Error_Page(Output, "既にこの村に参加しています。");
										elsif Village.State /= Villages.Prologue then
											Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
											Ase.Web.Header_Break(Output);
											Renderer.Message_Page(Output, Village_Id, Village'Access, "締め切りです。", User_Id, User_Password);
										elsif Villages.Lists.Created(User_Id, Village_List, Village_Id) then
											Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
											Ase.Web.Header_Break(Output);
											Renderer.Message_Page(Output, Village_Id, Village'Access, "自分の作成した村に入ってください。", User_Id, User_Password);
										elsif Village.Day_Duration >= 24 * 60 * 60.0
											and then Villages.Lists.Joined(User_Id, Village_List, Long_Only => True) 
										then
											Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
											Ase.Web.Header_Break(Output);
											Renderer.Message_Page(Output, Village_Id, Village'Access, "既に他の村に参加しています。", User_Id, User_Password);
										else
											declare
												Work_Num : Integer := Natural'Value(Ase.Web.Element(Inputs, "work"));
												Name_Num : constant Natural := Natural'Value(Ase.Web.Element(Inputs, "name"));
												Request : constant Villages.Requested_Role := Villages.Requested_Role'Value(Ase.Web.Element(Inputs, "request"));
												Cast : Villages.Casts.Cast_Type;
											begin
												Villages.Casts.Load(Cast);
												Villages.Casts.Exclude_Taken(Cast, Village);
												declare
													Person_Template : Villages.Person_Type renames Cast.People(Name_Num);
													Selected_Work : Villages.Casts.Work;
												begin
													if Work_Num < 0 then
														Searching_Established_Work : for I in Cast.Works'Range loop
															if Person_Template.Work = Cast.Works(I).Name then
																Work_Num := I;
																exit Searching_Established_Work;
															end if;
														end loop Searching_Established_Work;
													end if;
													if Work_Num < 0 then
														Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
														Ase.Web.Header_Break(Output);
														Renderer.Message_Page(Output, Village_Id, Village'Access, 
															"申し訳ありませんが既定の肩書き(" & (+Person_Template.Work) & ")は既に取られています。", 
															User_Id, User_Password);
													else
														Selected_Work := Cast.Works(Work_Num);
														if Person_Template.Name = "" or Selected_Work.Name = "" then
															Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
															Ase.Web.Header_Break(Output);
															Renderer.Message_Page(Output, Village_Id, Village'Access, "既に取られています。", User_Id, User_Password);
														elsif Selected_Work.Sex /= Villages.Neutral and then Selected_Work.Sex /= Person_Template.Sex then
															Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
															Ase.Web.Header_Break(Output);
															Renderer.Message_Page(Output, Village_Id, Village'Access, "性別と肩書きが一致しません。", User_Id, User_Password);
														elsif Selected_Work.Nominated and then Selected_Work.Name /= Person_Template.Work then
															Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
															Ase.Web.Header_Break(Output);
															Renderer.Message_Page(Output, Village_Id, Village'Access, "その肩書きは特定の組み合わせでしか使えません。", User_Id, User_Password);
														elsif Villages.Already_Joined_Another_Sex(Village, User_Id, Person_Template.Sex) then
															Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
															Ase.Web.Header_Break(Output);
															Renderer.Message_Page(Output, Village_Id, Village'Access, "以前にエントリされたときと性別が異なります。", User_Id, User_Password);
														else
															Append(Village.People, Villages.Person_Type'(Ada.Finalization.Controlled with
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
																Records => new Villages.Person_Record_Array'(0 => Villages.Default_Person_Record)));
															Add(Village, Villages.Join, Subject => Village.People'Last);
															Villages.Save(Village_Id, Village);
															Tabula.Villages.Lists.Managing.Refresh_Village_List;
															Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
															Ase.Web.Header_Break(Output);
															Renderer.Message_Page(Output, Village_Id, Village'Access, "村に参加しました。", User_Id, User_Password);
														end if;
													end if;
												end;
											end;
										end if;
									end;
								elsif Cmd = "narration" then
									if User_Id /= Tabula.Users.Administrator then
										Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
										Ase.Web.Header_Break(Output);
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
									if Village.State /= Villages.Prologue then
										Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
										Ase.Web.Header_Break(Output);
										Renderer.Error_Page(Output, "除名を行えるのはプロローグのみです。");
									elsif User_Id /= Tabula.Users.Administrator then
										Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
										Ase.Web.Header_Break(Output);
										Renderer.Error_Page(Output, "administratorのみに許された操作です。");
									else
										declare
											Target : constant Integer := Integer'Value(Ase.Web.Element(Inputs, "target"));
											Removed_Id : constant String := +Village.People(Target).Id;
										begin
											Villages.Escape(Village, Target, Now);
											Villages.Save(Village_Id, Village);
											Tabula.Villages.Lists.Managing.Refresh_Village_List;
											Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
											Ase.Web.Header_Break(Output);
											Renderers.Message_Page(Renderer, Output,
												Village_Id, Village'Access, 
												Removed_Id & "を村から除名しました。", 
												User_Id, User_Password);
										end;
									end if;
								elsif Player < 0 then
									Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
									Ase.Web.Header_Break(Output);
									Renderer.Error_Page(Output, "参加していません。");
								elsif Cmd = "commit" then
									if Village.Time = Villages.Night then
										Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
										Ase.Web.Header_Break(Output);
										Renderers.Message_Page(Renderer, Output, Village_Id, Village'Access, "今は行動を終えられない時間帯です。", User_Id, User_Password);
									elsif Village.People(Player).Records(Village.Today).State = Villages.Died then
										Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
										Ase.Web.Header_Break(Output);
										Renderer.Error_Page(Output, "死者は行動を決定できません。");
									else
										if not Village.People(Player).Commited then
											Village.People(Player).Commited := True;
											declare
												Changed, List_Changed : Boolean;
											begin
												Villages.Advance(Village, Now, Generator,
													Changed => Changed, List_Changed => List_Changed);
												Villages.Save(Village_Id, Village);
												if List_Changed then
													Tabula.Villages.Lists.Managing.Refresh_Village_List;
												end if;
											end;
										end if;
										Render_Reload_Page;
									end if;
								elsif Cmd = "rollback" then
									if Village.People(Player).Commited then
										Village.People(Player).Commited := False;
										Villages.Save(Village_Id, Village);
									end if;
									Render_Reload_Page;
								elsif Cmd = "escape" then
									if Village.State /= Villages.Prologue then
										Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
										Ase.Web.Header_Break(Output);
										Renderer.Error_Page(Output, "村から出られるのはプロローグのみです。");
									else
										declare
											OK : Boolean := False;
										begin
											begin
												declare
													X : constant Integer := Integer'Value(Ase.Web.Element(Inputs, "x"));
													Y : constant Integer := Integer'Value(Ase.Web.Element(Inputs, "y"));
													Z : constant Integer := Integer'Value(Ase.Web.Element(Inputs, "z"));
												begin
													OK := X + Y = Z;
												end;
											exception
												when Constraint_Error =>
													Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
													Ase.Web.Header_Break(Output);
													Renderers.Message_Page(Renderer, Output, Village_Id, Village'Access, "答えを入力してください。", User_Id, User_Password);
													goto Exit_Answer;
											end;
											if OK then
												Villages.Escape(Village, Player, Now);
												Villages.Save(Village_Id, Village);
												Tabula.Villages.Lists.Managing.Refresh_Village_List;
												Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
												Ase.Web.Header_Break(Output);
												Renderers.Message_Page(Renderer, Output, Village_Id, Village'Access, "旅に出ました。", User_Id, User_Password);
											else
												Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
												Ase.Web.Header_Break(Output);
												Renderers.Message_Page(Renderer, Output, Village_Id, Village'Access, "答えが違います。", User_Id, User_Password);
											end if;
											<<Exit_Answer>> null;
										end;
									end if;
								elsif Cmd = "action" then
									declare
										Action : String renames Ase.Web.Element(Inputs, "action");
										Said : Villages.Message_Counts renames Villages.Count_Messages(Village, Village.Today);
										Target : constant Integer := Integer'Value(Ase.Web.Element(Inputs, "target"));
									begin
										if Target < 0 or else Action = "" then
											Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
											Ase.Web.Header_Break(Output);
											Renderers.Message_Page(Renderer, Output, Village_Id, Village'Access, "対象と行動を選んでください。", User_Id, User_Password);
										elsif Action = "wake" then
											if Said(Player).Wake > 0 then
												Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
												Ase.Web.Header_Break(Output);
												Renderers.Error_Page(Renderer, Output,
													"人を起こせるのは一日一度です。");
											elsif not Village.People(Target).Commited then
												Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
												Ase.Web.Header_Break(Output);
												Renderers.Message_Page(Renderer, Output, Village_Id, Village'Access, "相手はまだ行動を終えていません。", User_Id, User_Password);
											else
												Village.People(Target).Commited := False;
												Add(Village, Villages.Action_Wake, Subject => Player, Target => Target);
												Villages.Save(Village_Id, Village);
												Render_Reload_Page;
											end if;
										elsif Action = "encourage" then
											if Said(Player).Encourage > 0 then
												Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
												Ase.Web.Header_Break(Output);
												Renderers.Error_Page(Renderer, Output,
													"話の続きを促せるのは一日一度です。");
											else
												Add(Village, Villages.Action_Encourage, Subject => Player, Target => Target);
												Villages.Save(Village_Id, Village);
												Render_Reload_Page;
											end if;
										elsif Action = "vampire_gaze" then
											if Village.People(Player).Role not in Villages.Vampire_Role then
												Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
												Ase.Web.Header_Break(Output);
												Renderers.Error_Page(Renderer, Output, "見つめられるのは吸血鬼だけです。");
											elsif Village.People(Target).Role in Villages.Vampire_Role then
												Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
												Ase.Web.Header_Break(Output);
												Renderers.Message_Page(Renderer, Output, Village_Id, Village'Access, "見つめようとした相手は吸血鬼です。", User_Id, User_Password);
											elsif Said(Player).Vampire_Gaze > 0 then
												Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
												Ase.Web.Header_Break(Output);
												Renderers.Error_Page(Renderer, Output, "見つめられるのは一日一度です。");
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
											Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
											Ase.Web.Header_Break(Output);
											Renderer.Error_Page(Output, "サポートされていない種類のアクションを行おうとしました。");
										end if;
									end;
								elsif Cmd = "speech" then
									if Village.State = Villages.Opened 
										and then Village.People(Player).Records(Village.Today).State = Villages.Died 
									then
										Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
										Ase.Web.Header_Break(Output);
										Renderers.Message_Page(Renderer, Output, Village_Id, Village'Access, "あなたは死にました。", User_Id, User_Password);
									elsif Village.Time /= Villages.Daytime then
										Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
										Ase.Web.Header_Break(Output);
										Renderers.Message_Page(Renderer, Output,
											Village_Id, Village'Access, "今は喋れない時間帯です。", User_Id, User_Password);
									else
										declare
											Text : String renames Renderers.Get_Text(Renderer, Inputs);
										begin
											if Text'Length > 0 then
												Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
												Ase.Web.Header_Break(Output);
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
									if Village.State = Villages.Opened 
										and then Village.People(Player).Records(Village.Today).State = Villages.Died 
									then
										Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
										Ase.Web.Header_Break(Output);
										Renderers.Message_Page(Renderer, Output, Village_Id, Village'Access, "あなたは死にました。", User_Id, User_Password);
									elsif Village.Time /= Villages.Daytime then
										Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
										Ase.Web.Header_Break(Output);
										Renderers.Message_Page(Renderer, Output, Village_Id, Village'Access, "今は喋れない時間帯です。", User_Id, User_Password);
									else
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
								elsif Cmd = "monologue" then
									if Village.State = Villages.Epilogue then
										Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
										Ase.Web.Header_Break(Output);
										Renderer.Error_Page(Output, "エピローグでは独白は喋れません。");
									else
										declare
											Text : String renames Renderers.Get_Text(Renderer, Inputs);
										begin
											if Text'Length > Max_Length_Of_Message then
												Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
												Ase.Web.Header_Break(Output);
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
									if Village.State = Villages.Epilogue then
										Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
										Ase.Web.Header_Break(Output);
										Renderer.Error_Page(Output, "エピローグでは呻けません。");
									elsif Village.People(Player).Records(Village.Today).State /= Villages.Died then
										Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
										Ase.Web.Header_Break(Output);
										Renderer.Error_Page(Output, "生者は呻けません。");
									else
										declare
											Text : String renames Renderers.Get_Text(Renderer, Inputs);
										begin
											if Text'Length > Max_Length_Of_Message then
												Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
												Ase.Web.Header_Break(Output);
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
											Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
											Ase.Web.Header_Break(Output);
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
											if Text /= Village.People(Player).Records(Village.Today).Note then
												if Village.Time = Villages.Night
													and then Village.People(Player).Role in Villages.Vampire_Role
													and then Village.People(Player).Records(Village.Today).State /= Villages.Died
												then
													if Villages.Unfortunate(Village) then
														for I in reverse Village.Messages.First_Index .. Village.Messages.Last_Index loop
															declare
																Message : Villages.Message renames Element(Village.Messages, I);
															begin
																if Message.Day < Village.Today then
																	Append(Village.Messages, Villages.Message'(
																		Kind => Villages.Howling_Blocked,
																		Day => Village.Today,
																		Time => Now,
																		Subject => -1,
																		Target => -1,
																		Text => Ada.Strings.Unbounded.Null_Unbounded_String));
																	exit;
																end if;
																exit when Message.Kind = Villages.Howling_Blocked;
															end;
														end loop;
														Village.People(Player).Records(Village.Today - 1).Note := +Text;
													else
														Append(Village.Messages, Villages.Message'(
															Kind => Villages.Howling,
															Day => Village.Today,
															Time => Now,
															Subject => Player,
															Target => -1,
															Text => +Text));
													end if;
												else
													Village.People(Player).Records(Village.Today).Note := +Text;
												end if;
												Villages.Save(Village_Id, Village);
											end if;
											Render_Reload_Page;
										end if;
									end;
								elsif Cmd = "vote" then
									if Village.Time = Villages.Night then
										Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
										Ase.Web.Header_Break(Output);
										Renderer.Message_Page(Output, Village_Id, Village'Access, "今は投票できない時間帯です。", User_Id, User_Password);
									else
										declare
											Target : constant Integer := Integer'Value(Ase.Web.Element(Inputs, "target"));
										begin
											if Target >= 0 and then Village.People(Target).Records(Village.Today).State = Villages.Died then
												Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
												Ase.Web.Header_Break(Output);
												Renderer.Error_Page(Output, "死者に投票はできません。");
											else
												if Target /= Village.People(Player).Records(Village.Today).Vote then
													Village.People(Player).Records(Village.Today).Vote := Target;
													Villages.Save(Village_Id, Village);
												end if;
												Render_Reload_Page;
											end if;
										end;
									end if;
								elsif Cmd = "target" then
									declare
										Target : constant Integer := Integer'Value(Ase.Web.Element(Inputs, "target"));
										Special : constant Boolean := Ase.Web.Checkbox_Value(Ase.Web.Element(Inputs, "special"));
										Target_Day : Natural := Village.Today;
										Special_Used : Boolean := False;
									begin
										if Village.Time = Villages.Night then
											Target_Day := Target_Day - 1;
										end if;
										for I in 0 .. Target_Day - 1 loop
											if Village.People(Player).Records(I).Special then
												Special_Used := True;
											end if;
										end loop;
										if Special_Used and Special then
											Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
											Ase.Web.Header_Break(Output);
											Renderer.Error_Page(Output, "銀の弾丸は一発限りです。");
										elsif Village.Daytime_Preview /= Villages.None 
											and then (Village.People(Player).Role = Villages.Detective 
											or else Village.People(Player).Role = Villages.Doctor)
										then
											if Village.Time = Villages.Night then
												Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
												Ase.Web.Header_Break(Output);
												Renderer.Message_Page(Output, Village_Id, Village'Access, "医者と探偵は、夜に能力を使えません。", User_Id, User_Password);
											elsif Village.People(Player).Records(Target_Day).Target >= 0 then
												Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
												Ase.Web.Header_Break(Output);
												Renderer.Error_Page(Output, "医者と探偵の行動選択は一日に一度しかできません。");
											elsif Target < 0 then
												Render_Reload_Page;
											else
												Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
												Ase.Web.Header_Break(Output);
												Renderers.Target_Page(Renderer, Output,
													Village_Id,
													Village,
													Player => Player,
													Target => Target,
													User_Id => User_Id, 
													User_Password => User_Password);
											end if;
										else
											if Target /= Village.People(Player).Records(Target_Day).Target 
												or else Special /= Village.People(Player).Records(Target_Day).Special 
											then
												Village.People(Player).Records(Target_Day).Target := Target;
												Village.People(Player).Records(Target_Day).Special := Special;
												Villages.Save(Village_Id, Village);
											end if;
											Render_Reload_Page;
										end if;
									end;
								elsif Cmd = "target2" then
									if Village.Time = Villages.Night then
										Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
										Ase.Web.Header_Break(Output);
										Renderer.Message_Page(Output, Village_Id, Village'Access, "医者と探偵は、夜に能力を使えません。", User_Id, User_Password);
									else
										declare
											Target : constant Integer := Integer'Value(Ase.Web.Element(Inputs, "target"));
										begin
											case Village.People(Player).Role is
												when Villages.Doctor =>
													if Village.People(Target).Role = Villages.Gremlin then
														Add(Village, Villages.Doctor_Found_Gremlin_Preview, Subject => Player, Target => Target);
													elsif Village.People(Target).Records(Village.Today).State = Villages.Infected then
														declare
															Result : Villages.Message_Kind := Villages.Doctor_Cure_Preview;
														begin
															if Village.Doctor_Infected = Villages.Find_Infection
																and then Village.People(Player).Records(Village.Today).State = Villages.Infected
															then
																Result := Villages.Doctor_Found_Infection_Preview;
															end if;
															Add(Village, Result, Subject => Player, Target => Target);
														end;
													else
														Add(Village, Villages.Doctor_Failed_Preview, Subject => Player, Target => Target);
													end if;
													Village.People(Player).Records(Village.Today).Target := Target;
													Villages.Save(Village_Id, Village);
													Render_Reload_Page;
												when Villages.Detective =>
													Add(Village, Villages.Detective_Survey_Preview, 
														Subject => Player, Target => Target, 
														Text => +Village.People(Target).Records(Village.Today).Note);
													Village.People(Player).Records(Village.Today).Target := Target;
													Villages.Save(Village_Id, Village);
													Render_Reload_Page;
												when others =>
													Ase.Web.Header(Output, Ase.Web.HTML);
													Ase.Web.Header_Break(Output);
													Renderer.Error_Page(Output, "医者と探偵以外は、日中に能力を使えません。");
											end case;
										end;
									end if;
								elsif Cmd = "rule" then
									if Village.Today > 0 then
										Ase.Web.Header(Output, Ase.Web.HTML, Cookie, Now + Cookie_Duration); 
										Ase.Web.Header_Break(Output);
										Renderer.Message_Page(Output, Village_Id, Village'Access, "開始以降ルールは変更できません。", User_Id, User_Password);
									else
										Renderers.Rule.Change(Village, Inputs);
										Villages.Save(Village_Id, Village);
										Render_Reload_Page;
									end if;
								else
									Ase.Web.Header(Output, Ase.Web.HTML);
									Ase.Web.Header_Break(Output);
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
	when Ase.Directories.Lock_Files.Lock_Error =>
		Ase.Web.Header(Output, Ase.Web.Text);
		Ase.Web.Header_Break(Output);
		Write(Output, "White fog. Wait 1 minute!" & Ase.Web.Line_Break);
	when E : others =>
		Ase.Web.Header(Output, Ase.Web.Text);
		Ase.Web.Header_Break(Output);
		Write(Output, Ada.Exceptions.Exception_Information(E));
		Write(Output, Ase.Web.Line_Break);
end Vampire;
