-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Calendar.Formatting;
with Ada.Containers;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Ase.Editing;
with Ase.Numerics.MT19937;
with Ase.Strings.Lists;
with Tabula.Calendar;
with Tabula.Renderers.Rule;
with Tabula.Villages.Casts;
with Tabula.Villages.Casts.Load;
package body Tabula.Renderers is
	
	use type Ada.Strings.Unbounded.Unbounded_String;
	
	function "+" (S : Ada.Strings.Unbounded.Unbounded_String) return String renames Ada.Strings.Unbounded.To_String;
	function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String renames Ada.Strings.Unbounded.To_Unbounded_String;
	
	function To_String(X : Integer) return String is
	begin
		return Ase.Editing.Image(Item => X);
	end To_String;
	
	type Stage_Kind is (A_Village, A_Castle);
	type Stage_Type is record
		Introduction, Breakdown : not null access constant String;
	end record;
	
	Stages : constant array(Stage_Kind) of Stage_Type := (
		A_Village => (
			Introduction => new String'(
				"いつものように、血塗られた伝説が残る村を照らすお日さまが傾きかけました。 " & 
				"慌ただしい人も暇な人も、子供もお年寄りも、一日の仕事を終えて、村人たちが酒場に集まる時間です。" & Line_Break &
				"今夜は地主さんもやって来ますので、出迎えの準備もしなければなりません……。 "),
			Breakdown => new String'(
				"昨夜ついに来ることはなかった地主さんの死体が、翌朝村の真ん中にありました。" & Line_Break &
				"死体は干乾び、首筋には牙のあとがありますが、これは獣のものではありません……。 " &
				"村人たちが不審がっていると、突如地主さんの死体が、赤い目を見開き牙を剥いて起き上がりました。 " &
				"しかし山間から差し込む朝日を浴びてその身体は灰となり崩れ落ちてゆきます……。" & Line_Break &
				"疑う余地はありません。 " &
				"吸血鬼は実在し……この村に紛れているのです！" & Line_Break &
				"誰かが古びた杭を持って来ました……これしかないのでしょうか……。 ")),
		A_Castle => (
			Introduction => new String'(
				"伝説が残る古城の前に、まばらな人々があつまってきました。 " &
				"観光客、学術の徒、それを案内する地元の村人、小銭を稼ごうとする物売り、遊び場に来た子供たち、家の無い浮浪者など……。 " & Line_Break &
				"管財人が錆びた鍵を回しました。 " &
				"数百年ぶりに、重く閉ざされた扉は開かれます。 "),
			Breakdown => new String'(
				"物珍しそうに城内を見物していた人々は、背後に音を聞きました。 " &
				"ふりかえると扉が閉ざされています。 " & Line_Break &
				"慌てて二階に駆け上がり、我先にと鉄格子のはめられた窓から外を覗くと、血に染まり倒れた管財人の体が、足の先指の先から灰と化しています。 " &
				"その脇から何者かの影が飛び上がりました。 " &
				"人々は周囲を見回し、天井を見上げましたが、弱い光が微かに差し込む天窓以外は、全て格子窓になっていました。 " & Line_Break &
				"人数が足りないぞ！誰かが叫びました。 " &
				"人々は広間に戻り、点呼を取り直しましたが、誰も欠けていません。 " &
				"そもそも誰があの高い天窓から出入りできたというのでしょう。 " & Line_Break &
				"改めて城内を探索しますと、古の領主が残した拷問や処刑を行うための悪趣味な道具がごろごろしています。 " &
				"こうして、古城での日々がはじまりました……。 ")));
	
	function Stage(Village : in Villages.Village_Type) return Stage_Kind is
		L : constant Natural := Ada.Strings.Unbounded.Length(Village.Name);
	begin
		if L >= 3 and then Ada.Strings.Unbounded.Slice(Village.Name, L - 2, L) = "城" then
			return A_Castle;
		else
			return A_Village;
		end if;
	end Stage;
	
	function Image(Role : Villages.Requested_Role) return String is
		use Villages;
	begin
		case Role is
			when Inhabitant => return "村人";
			when Vampire => return "吸血鬼";
			when Servant => return "使徒";
			when Detective => return "探偵";
			when Astronomer => return "天文家";
			when Doctor => return "医者";
			when Hunter => return "猟師";
			when Sweetheart => return "恋人";
			when Random => return "ランダム";
			when Rest => return "希望無し";
			when Village_Side => return "村側";
			when Vampire_Side => return "吸血鬼側";
			when Gremlin => return "妖魔";
		end case;
	end Image;
	
	function Image(Role : Villages.Person_Role) return String is
		use Villages;
	begin
		case Role is
			when Inhabitant | Loved_Inhabitant => return "善良な村人";
			when Unfortunate_Inhabitant => return "数奇な運命の村人";
			when Vampire_K | Vampire_Q | Vampire_J => return "吸血鬼";
			when Servant => return "吸血鬼の使徒";
			when Werewolf => return "人狼";
			when Possessed => return "人狼の下僕";
			when Detective => return "探偵";
			when Astronomer => return "天文家";
			when Doctor => return "医者";
			when Hunter => return "猟師";
			when Lover | Sweetheart_M | Sweetheart_F => return "恋する村人";
			when Gremlin => return "妖魔";
		end case;
	end Image;
	
	function Name(Person : Villages.Person_Type) return String is
	begin
		return (+Person.Work) & (+Person.Name);
	end Name;
	
	Role_Image_File_Name : constant array(Villages.Person_Role) of not null access constant String := (
		Villages.Gremlin => new String'("gremlin.png"),
		Villages.Vampire_Role => new String'("vampire.png"),
		Villages.Werewolf => new String'("werewolf.png"),
		Villages.Possessed => new String'("possessed.png"),
		Villages.Servant => new String'("servant.png"),
		Villages.Inhabitant | Villages.Loved_Inhabitant | Villages.Unfortunate_Inhabitant => new String'("inhabitant.png"),
		Villages.Detective => new String'("detective.png"),
		Villages.Doctor => new String'("doctor.png"),
		Villages.Astronomer => new String'("astronomer.png"),
		Villages.Hunter => new String'("hunter.png"),
		Villages.Lover | Villages.Sweetheart_M | Villages.Sweetheart_F => new String'("sweetheart.png"));
	
	procedure Handle_List(
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String;
		Template : in Ase.Web.Producers.Template;
		Object : in Renderer; 
		Village_List : in Villages.Lists.Village_Lists.Vector;
		Log_Limits : in Natural;
		User_Id, User_Password : in String)
	is
		use Villages.Lists.Village_Lists;
		use type Villages.Village_State;
		Log : Boolean;
		procedure Handle_Villages(Output : not null access Ada.Streams.Root_Stream_Type'Class;
			Tag : in String; Template : in Ase.Web.Producers.Template) 
		is
			First : Natural := Village_List.First_Index;
		begin
			if Log then
				declare
					C : Natural := Log_Limits;
				begin
					for I in reverse First .. Village_List.Last_Index loop
						if Element(Village_List, I).State = Villages.Closed then
							C := C - 1;
							if C = 0 then
								First := I;
								exit;
							end if;
						end if;
					end loop;
				end;
			end if;
			for I in First .. Village_List.Last_Index loop
				declare
					Item : Villages.Lists.Village_List_Item renames Element(Village_List, I);
					procedure Handle_Village(Output : not null access Ada.Streams.Root_Stream_Type'Class;
						Tag : in String; Template : in Ase.Web.Producers.Template) is
					begin
						if Tag = "id" then
							Write(Output, Item.Id);
						elsif Tag = "name" then
							Write(Output, "<a href=");
							Link(Renderer'Class(Object), Output, Item.Id, Log => Log, 
								User_Id => User_Id, User_Password => User_Password);
							Write(Output, ">");
							if Item.Day_Duration < 24 * 60 * 60.0 then
								Write(Output, "短期 ");
							end if;
							Write(Output, Ase.Web.Markup_Entity(+Item.Name));
							Write(Output, "</a>");
						elsif Tag = "people" then
							Write(Output, To_String(Natural(Item.People.Length)) & "人");
						elsif Tag = "day" then
							Day_Name(Renderer'Class(Object), Output, Item.Today, Item.Today, Item.State);
						else
							raise Program_Error with "Invalid template """ & Tag & """";
						end if;
					end Handle_Village;
				begin
					if (Item.State = Villages.Closed) = Log then
						Ase.Web.Producers.Produce(Output, Template, Handler => Handle_Village'Access);
					end if;
				end;
			end loop;
		end Handle_Villages;
	begin
		if Tag = "villages" then
			for I in Village_List.First_Index .. Village_List.Last_Index loop
				declare
					Item : Villages.Lists.Village_List_Item renames Element(Village_List, I);
				begin
					if Item.State /= Villages.Closed then
						Log := False;
						Ase.Web.Producers.Produce(Output, Template, Handler => Handle_Villages'Access);
						exit;
					end if;
				end;
			end loop;
		elsif Tag = "log" then
			for I in Village_List.First_Index .. Village_List.Last_Index loop
				declare
					Item : Villages.Lists.Village_List_Item renames Element(Village_List, I);
				begin
					if Item.State = Villages.Closed then
						Log := True;
						Ase.Web.Producers.Produce(Output, Template, Handler => Handle_Villages'Access);
						exit;
					end if;
				end;
			end loop;
		elsif Tag = "stylesheet" then
			Write(Output, "<link rel=""stylesheet"" type=""text/css"" href=");
			Link_Style_Sheet(Renderer'Class(Object), Output);
			Write(Output, "/>");
		elsif Tag = "background" then
			Link_Image(Renderer'Class(Object), Output, Object.Configuration.Background_Image_File_Name.all);
		elsif Tag = "back" then
			Write(Output, "<a href=");
			Link(Renderer'Class(Object), Output, User_Id => User_Id, User_Password => User_Password);
			Write(Output, '>');
			Ase.Web.Producers.Produce(Output, Template);
			Write(Output, "</a>");
		else
			raise Program_Error with "Invalid template """ & Tag & """";
		end if;
	end Handle_List;
	
	procedure Handle_Users(
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String;
		Template : in Ase.Web.Producers.Template;
		Object : in Renderer;
		Village_Id : in Villages.Lists.Village_Id := Villages.Lists.Invalid_Village_Id;
		User_Id : in String;
		User_Password : in String) is
	begin
		if Tag = "id" then
			Write(Output, '"');
			Write(Output, User_Id);
			Write(Output, '"');
		elsif Tag = "password" then
			Write(Output, '"');
			Write(Output, Ase.Web.Markup_Entity(User_Password));
			Write(Output, '"');
		elsif Tag = "uri" then
			Link(Renderer'Class(Object), Output, Village_Id => Village_Id,
				User_Id => User_Id, User_Password => User_Password);
		elsif Tag = "villageid" then
			if Village_Id = Villages.Lists.Invalid_Village_Id then
				Write(Output, """""");
			else
				Write(Output, '"');
				Write(Output, Village_Id);
				Write(Output, '"');
			end if;
		elsif Tag = "invillage" then
			declare
				procedure Handle(Output : not null access Ada.Streams.Root_Stream_Type'Class;
					Tag : in String; Template : in Ase.Web.Producers.Template) is
				begin
					Handle_Users(Output, Tag, Template, Object, Village_Id, User_Id, User_Password);
				end Handle;
			begin
				if Village_Id /= Villages.Lists.Invalid_Village_Id then
					Ase.Web.Producers.Produce(Output, Template, Handler => Handle'Access);
				end if;
			end;
		else
			raise Program_Error with "Invalid template """ & Tag & """";
		end if;
	end Handle_Users;
	
	procedure Handle_Villages(
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String;
		Template : in Ase.Web.Producers.Template;
		Object : in Renderer;
		Village_Id : in Villages.Lists.Village_Id;
		Village : in Villages.Village_Type;
		Day : in Natural;
		User_Id : in String;
		User_Password : in String)
	is
		use type Ada.Streams.Stream_Element_Count;
	begin
		if Tag = "title" then
			Write(Output, Ase.Web.Markup_Entity(+Village.Name));
			Write(Output, ' ');
			Day_Name(Renderer'Class(Object), Output, Day, Village.Today, Village.State);
			if Template.Is_Empty then
				Write(Output, " - ");
			end if;
		else
			Handle_Users(Output, Tag, Template, Object, 
				Village_Id => Village_Id, User_Id => User_Id, User_Password => User_Password);
		end if;
	end Handle_Villages;
	
	procedure Handle_Messages(
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String;
		Template : in Ase.Web.Producers.Template;
		Object : in Renderer;
		Village_Id : in Villages.Lists.Village_Id;
		Village : in Villages.Village_Type;
		Day : in Natural;
		Message : in Villages.Message;
		Time : in Ada.Calendar.Time;
		User_Id : in String;
		User_Password : in String)
	is
		use type Villages.Message_Kind;
		People : Villages.Person_Array_Access;
		Subject : Natural;
	begin
		case Message.Kind is
			when Villages.Escaped_Speech =>
				People := Village.Escaped_People;
				Subject := Message.Subject;
			when Villages.Detective_Message_Kind =>
				People := Village.People;
				Subject := Message.Target;
			when others =>
				People := Village.People;
				Subject := Message.Subject;
		end case;
		declare
			Person : Villages.Person_Type renames People(Subject);
		begin
			if Tag = "image" then
				Link_Image(Renderer'Class(Object), Output, +Person.Image);
			elsif Tag = "name" then
				Write(Output, Name(Person));
			elsif Tag = "time" then
				Write(Output, Ada.Calendar.Formatting.Image(Time, Time_Zone => Calendar.Time_Offset));
			elsif Tag = "text" then
				declare
					S : String renames Ada.Strings.Unbounded.To_String(Message.Text);
				begin
					if Ada.Strings.Unbounded.Length(Message.Text) > Max_Length_Of_Message then
						declare
							I : Natural := Max_Length_Of_Message;
						begin
							while Character'Pos(S(I)) >= 16#c0# 
								or else Character'Pos(S(I - 1)) >= 16#e0#
							loop
								I := I - 1;
							end loop;
							Write(Output, Ase.Web.Markup_HTML(S(1 .. I), HTML_Version(Renderer'Class(Object))));
							Write(Output, "<b>");
							Write(Output, Ase.Web.Markup_HTML(S(I + 1 .. S'Last), HTML_Version(Renderer'Class(Object))));
							Write(Output, "</b>");
						end;
					else
						Write(Output, Ase.Web.Markup_HTML(S, HTML_Version(Renderer'Class(Object))));
					end if;
				end;
			else
				Handle_Villages(Output, Tag, Template, Object,
					Village_Id, Village, Day, User_Id => User_Id, User_Password => User_Password);
			end if;
		end;
	end Handle_Messages;
	
	function Fatalities_List(Village : Villages.Village_Type; Day : Natural; Executed : Integer) return String is
		use Ada.Strings.Unbounded;
		use type Villages.Person_State;
		Result : Ada.Strings.Unbounded.Unbounded_String;
	begin
		pragma Assert(Day >= 2);
		for I in Village.People'Range loop
			declare
				P : Villages.Person_Type renames Village.People(I);
			begin
				if P.Records(Day - 1).State /= Villages.Died
					and then P.Records(Day).State = Villages.Died
					and then Executed /= I
				then
					if Result /= Ada.Strings.Unbounded.Null_Unbounded_String then
						Append(Result, "、");
					else
						Result := +"翌朝、";
					end if;
					Append(Result, Name(P));
				end if;
			end;
		end loop;
		if Result /= Ada.Strings.Unbounded.Null_Unbounded_String then
			Append(Result, "の遺体が見つかりました……！");
		end if;
		return +Result;
	end Fatalities_List;
	
	function Survivors_List(Village : Villages.Village_Type; Day : Natural) return String is
		use Ada.Strings.Unbounded;
		use type Villages.Person_State;
		Result : Ada.Strings.Unbounded.Unbounded_String;
	begin
		for I in Village.People'Range loop
			declare
				P : Villages.Person_Type renames Village.People(I);
			begin
				if P.Records(Day).State /= Villages.Died then
					if Result /= Ada.Strings.Unbounded.Null_Unbounded_String then
						Append(Result, "、");
					end if;
					Append(Result, Name(P));
				end if;
			end;
		end loop;
		Append(Result, "が生存者です。 ");
		return +Result;
	end Survivors_List;
	
	function Vampires_List(Village : Villages.Village_Type) return String is
		use Ada.Strings.Unbounded;
		use type Villages.Person_Role;
		Result : Unbounded_String := +"その夜、草木も眠る頃、人知れず月を舞う影がありました……。 ";
	begin
		for I in Villages.Vampire_Role loop
			for Position in Village.People'Range loop
				declare
					P : Villages.Person_Type renames Village.People(Position);
				begin
					if P.Role = I then
						if I /= Villages.Vampire_K then
							Append(Result, "、");
						end if;
						Append(Result, Name(P));
					end if;
				end;
			end loop;
		end loop;
		Append(Result, "。 村を見下ろすと、誰かが夜道を早足で歩いています……。 ");
		return +Result;
	end Vampires_List;
	
	function Breakdown_List(Village : Villages.Village_Type) return String is
		use Ada.Strings.Unbounded;
		use type Villages.Person_Role;
		use type Villages.Village_State;
		use type Villages.Monster_Side;
		use type Villages.Teaming;
		Detective, Astronomer, Doctor, Hunter, Sweetheart, Lover, Unfortunate, Servant, Gremlin : Boolean := False;
		Vampires : Natural := 0;
		Village_Side_Capabilityperson : Natural := 0;
		procedure Countup(Role : Villages.Person_Role) is
		begin
			case Role is
				when Villages.Detective => 
					Detective := True;
					Village_Side_Capabilityperson := Village_Side_Capabilityperson + 1;
				when Villages.Astronomer =>
					Astronomer := True;
					Village_Side_Capabilityperson := Village_Side_Capabilityperson + 1;
				when Villages.Doctor =>
					Doctor := True;
					Village_Side_Capabilityperson := Village_Side_Capabilityperson + 1;
				when Villages.Hunter =>
					Hunter := True;
					Village_Side_Capabilityperson := Village_Side_Capabilityperson + 1;
				when Villages.Lover =>
					Lover := True;
					Village_Side_Capabilityperson := Village_Side_Capabilityperson + 1;
				when Villages.Sweetheart_M | Villages.Sweetheart_F =>
					Sweetheart := True;
					Village_Side_Capabilityperson := Village_Side_Capabilityperson + 1;
				when Villages.Unfortunate_Inhabitant =>
					Unfortunate := True;
					Village_Side_Capabilityperson := Village_Side_Capabilityperson + 1;
				when Villages.Servant =>
					Servant := True;
				when Villages.Vampire_Role =>
					Vampires := Vampires + 1;
				when Villages.Gremlin =>
					Gremlin := True;
				when others =>
					null;
			end case;
		end Countup;
		Result : Ada.Strings.Unbounded.Unbounded_String;
	begin
		if Village.Victim_Existing then
			Result := "地主さんを含む" & (+To_String(1 + Village.People'Length));
			Countup(Village.Victim_Role);
		else
			Result := +To_String(Village.People'Length);
		end if;
		Append(Result, "人の村人の中には");
		for Position in Village.People'Range loop
			Countup(Village.People(Position).Role);
		end loop;
		if Village.Teaming in Villages.Hidings then
			Append(Result, To_String(Village_Side_Capabilityperson));
			Append(Result, "人の能力者");
		else
			if Detective then
				Append(Result, "、探偵");
			end if;
			if Astronomer then
				Append(Result, "、天文家");
			end if;
			if Doctor then
				Append(Result, "、医者");
			end if;
			if Hunter then
				Append(Result, "、猟師");
			end if;
			if Lover then
				Append(Result, "、片想い");
			end if;
			if Sweetheart then
				Append(Result, "、恋人");
			end if;
			if Unfortunate then
				Append(Result, "、数奇な運命の村人");
			end if;
		end if;
		Append(Result, "がいます。 ");
		if Village.Monster_Side = Villages.Shuffling then
			Append(Result, "吸血鬼の全貌はわかりません……。 ");
		else
			Append(Result, "そして昨夜、月明かりに照らし出された人影が");
			Append(Result, To_String(Vampires));
			Append(Result, "つ……。 ");
			if Servant then
				case Village.Servant_Knowing is
					when Villages.None =>
						Append(Result, "それを崇める者……。 ");
					when Villages.Vampire_K =>
						Append(Result, "吸血鬼の王を目撃し魅了された者……。 ");
					when Villages.Vampires =>
						Append(Result, "吸血鬼の集いを目撃し魅了された者……。 ");
				end case;
			end if;
			if Gremlin then
				Append(Result, "さらに忍び寄る魔の手……。 ");
			end if;
		end if;
		return +Result;
	end Breakdown_List;
	
	function Detective_Survey_Message(Village : Villages.Village_Type; Message : Villages.Message) return String is
		use Villages;
		Subject : Villages.Person_Type renames Village.People(Message.Subject);
		function Showing_Role(Role : Villages.Person_Role) return String is
		begin
			case Role is
				when Villages.Gremlin | Villages.Vampire_K .. Villages.Vampire_J => return "人間では無かった";
				when others => return Image(Role) & "だった";
			end case;
		end Showing_Role;
		Role : Villages.Person_Role;
	begin
		case Detective_Message_Kind(Message.Kind) is
			when Detective_Survey | Detective_Survey_Preview =>
				declare
					Target : Villages.Person_Type renames Village.People(Message.Target);
				begin
					if Village.Daytime_Preview = Message_Only and then Message.Kind = Detective_Survey_Preview then
						return Name(Subject) & "は" & Name(Target) & "を調査しました。";
					else
						Role := Target.Role;
						return Name(Subject) & "は" & Name(Target) & "を調査しました。" & Line_Break &
							"どうやら" & Showing_Role(Role) & "ようです。" & Line_Break;
					end if;
				end;
			when Detective_Survey_Victim =>
				return Name(Subject) & "は地主さんを調査しました。" & Line_Break &
					"どうやら" & Showing_Role(Village.Victim_Role) & "ようです。" & Line_Break;
		end case;
	end Detective_Survey_Message;
	
	function Doctor_Cure_Message(Village : Villages.Village_Type; Message : Villages.Message) return String is
		Subject : Villages.Person_Type renames Village.People(Message.Subject);
		Target : Villages.Person_Type renames Village.People(Message.Target);
		Showing_Result : constant array(Villages.Doctor_Message_Kind) of not null access constant String := (
			Villages.Doctor_Found_Infection | Villages.Doctor_Found_Infection_Preview |
			Villages.Doctor_Cure | Villages.Doctor_Cure_Preview =>
				new String'("を診察し、首筋に牙の跡を見つけました。" & Line_Break & "薬が効くことを祈りましょう。"),
			Villages.Doctor_Failed | Villages.Doctor_Failed_Preview =>
				new String'("を診察しましたが、異常は見当たりませんでした。"),
			Villages.Doctor_Found_Gremlin | Villages.Doctor_Found_Gremlin_Preview =>
				new String'("を診察しました。" & Line_Break & "……妖魔だ！"));
	begin
		return Name(Subject) & "は" & Name(Target) & Showing_Result(Message.Kind).all;
	end Doctor_Cure_Message;
	
	function Astronomer_Observation_Message(Village : Villages.Village_Type; Message : Villages.Message) return String is
		use Villages;
		Subject : Villages.Person_Type renames Village.People(Message.Subject);
		Target : Villages.Person_Type renames Village.People(Message.Target);
		function Showing_Result return String is
		begin
			if Target.Role in Vampire_Role 
				or else Target.Role = Gremlin 
				or else Target.Records(Message.Day - 1).State = Infected 
			then
				return "観測していて、人影が飛び立つのを目撃してしまいました。";
			else
				return "観測していました。";
			end if;
		end Showing_Result;
	begin
		return Name(Subject) & "は" & Name(Target) & "の家の上空を" & Showing_Result;
	end Astronomer_Observation_Message;
	
	function Hunter_Guard_Message(Village : Villages.Village_Type; Message : Villages.Message) return String is
		use Villages;
		Subject : Villages.Person_Type renames Village.People(Message.Subject);
	begin
		case Message.Kind is
			when Hunter_Nothing_With_Silver =>
				return Name(Subject) & "は銃に銀の弾丸を込めていましたが、その夜は何事もありませんでした。";
			when Hunter_Infected_With_Silver =>
				return Name(Subject) & "は銀の弾丸で吸血鬼を撃ち抜きました。";
			when Hunter_Killed_With_Silver =>
				return Name(Subject) & "は自らの命と引き換えに、銀の弾丸で吸血鬼を撃ち抜きました。";
			when others =>
				declare
					Target : Villages.Person_Type renames Village.People(Message.Target);
				begin
					case Hunter_Message_Kind(Message.Kind) is
						when Hunter_Guard => 
							return Name(Subject) & "は" & Name(Target) & "を吸血鬼から守り抜きました。";
						when Hunter_Guard_With_Silver => 
							return Name(Subject) & "は" & Name(Target) & "を守り、銀の弾丸で吸血鬼を撃ち抜きました。";
						when Hunter_Nothing_With_Silver | Hunter_Infected_With_Silver | Hunter_Killed_With_Silver => 
							raise Program_Error;
						when Hunter_Failed => 
							return Name(Subject) & "は" & Name(Target) & "を守っていました。";
						when Hunter_Failed_With_Silver => 
							return Name(Subject) & "は" & Name(Target) & "を銀の弾丸で守っていました。";
					end case;
				end;
		end case;
	end Hunter_Guard_Message;

	function Servant_Knew_Message(Village : Villages.Village_Type; Message : Villages.Message) return String is
		use Ada.Strings.Unbounded;
		use Villages;
		Subject : Person_Type renames Village.People(Message.Subject);
		Result : Ada.Strings.Unbounded.Unbounded_String := +(Name(Subject) & "は見てしまいました。");
	begin
		case Servant_Message_Kind(Message.Kind) is
			when Servant_Knew_Vampire_K =>
				Append(Result, "吸血鬼の王は");
				for Position in Village.People'Range loop
					declare
						P : Person_Type renames Village.People(Position);
					begin
						if P.Role = Villages.Vampire_K then
							Append(Result, Name(P));
						end if;
					end;
				end loop;
				Append(Result, "です。");
			when Servant_Knew_Vampires =>
				Append(Result, "吸血鬼は");
				for Role in Villages.Vampire_Role loop
					for Position in Village.People'Range loop
						declare
							P : Person_Type renames Village.People(Position);
						begin
							if P.Role = Role then
								Append(Result, Name(P));
							end if;
						end;
					end loop;
				end loop;
				Append(Result, "です。");
		end case;
		return +Result;
	end Servant_Knew_Message;
	
	function Vampire_Murder_Message(Village : Villages.Village_Type; Message : Villages.Message;
		Executed : Integer) return String 
	is
		use Ada.Strings.Unbounded;
		use type Villages.Person_Role;
		Result : Ada.Strings.Unbounded.Unbounded_String;
		Subject : Villages.Person_Type renames Village.People(Message.Subject);
		Target : Villages.Person_Type renames Village.People(Message.Target);
	begin
		if Subject.Role in Villages.Vampire_Role then
			for Role in Villages.Vampire_Role loop
				for I in Village.People'Range loop
					if I /= Executed then
						declare
							P : Villages.Person_Type renames Village.People(I);
						begin
							if P.Role = Role then
								declare
									V : constant Integer := P.Records(Message.Day - 1).Target;
								begin
									if V >= 0 then
										declare
											T : Villages.Person_Type renames Village.People(V);
										begin
											Append(Result, Name(P) & "は" & Name(T) & "に目をつけました。" & Line_Break);
										end;
									end if;
								end;
							end if;
						end;
					end if;
				end loop;
			end loop;
		else
			Append(Result, Name(Subject) & "は" & Name(Target) & "に目をつけました。" & Line_Break);
		end if;
		Append(Result, "吸血鬼は" & Name(Target) & "を");
		case Villages.Vampire_Message_Kind(Message.Kind) is
			when Villages.Vampire_Murder =>
				Append(Result, "襲いました。");
			when Villages.Vampire_Murder_And_Killed =>
				Append(Result, "襲い、抵抗を受け殺されました。");
			when Villages.Vampire_Infection =>
				Append(Result, "感染させました。");
			when Villages.Vampire_Infection_And_Killed =>
				Append(Result, "感染させ、抵抗を受け殺されました。");
			when Villages.Vampire_Failed =>
				Append(Result, "襲おうとしましたが、何者かに妨げられました。");
			when Villages.Vampire_Failed_And_Killed =>
				Append(Result, "襲おうとしましたが、何者かに妨げられ殺されました。");
		end case;
		return +Result;
	end Vampire_Murder_Message;
	
	function Get_Village_Id(
		Object : Renderer; 
		Query_Strings : Ase.Web.Query_Strings) return Villages.Lists.Village_Id
	is
		S : String renames Ase.Web.Element(Query_Strings, "village");
	begin
		if S'Length = Villages.Lists.Village_Id'Length then
			return S;
		else
			return Villages.Lists.Invalid_Village_Id;
		end if;
	end Get_Village_Id;
	
	procedure Get_Day(
		Object : in Renderer; 
		Village : in Villages.Village_Type; 
		Query_Strings : in Ase.Web.Query_Strings; 
		Day : out Natural)
	is
		use type Villages.Village_State;
		S : String renames Ase.Web.Element(Query_Strings, "day");
	begin
		Day := Natural'Value(S);
	exception
		when Constraint_Error => 
			if Village.State /= Villages.Closed then
				Day := Village.Today;
			else
				Day := 0;
			end if;
	end Get_Day;
	
	procedure Get_Range(
		Object : in Renderer; 
		Village : in Villages.Village_Type; 
		Day : in Natural;
		Query_Strings : in Ase.Web.Query_Strings; 
		First, Last : out Integer) is
	begin
		First := -1;
		Last := -1;
	end Get_Range;
	
	function Get_User_Id(
		Object : Renderer; 
		Query_Strings : Ase.Web.Query_Strings;
		Cookie : Ase.Web.Cookie) return String is
	begin
		return Ase.Web.Element(Cookie, "id");
	end Get_User_Id;
	
	function Get_User_Password(
		Object : Renderer; 
		Query_Strings : Ase.Web.Query_Strings;
		Cookie : Ase.Web.Cookie) return String is
	begin
		return Ase.Web.Element(Cookie, "password");
	end Get_User_Password;
	
	procedure Set_User(
		Object : in Renderer; 
		Cookie : in out Ase.Web.Cookie;
		New_User_Id: in String;
		New_User_Password : in String) is
	begin
		Ase.Web.String_Maps.Include(Cookie, "id", New_User_Id);
		Ase.Web.String_Maps.Include(Cookie, "password", New_User_Password);
	end Set_User;
	
	function Get_Text(
		Object : Renderer; 
		Inputs : Ase.Web.Query_Strings) return String is
	begin
		return Ase.Strings.Trim(Ase.Web.Element(Inputs, "text"), Ada.Strings.Both);
	end Get_Text;
	
	function Is_User_Page(
		Object : Renderer; 
		Query_Strings : Ase.Web.Query_Strings;
		Cookie : Ase.Web.Cookie) return Boolean 
	is
		User_Id : String renames Ase.Web.Element(Cookie, "id");
	begin
		if User_Id = "" then
			return False;
		else
			return Ase.Web.Element(Query_Strings, "user") = User_Id;
		end if;
	end Is_User_Page;
	
	-- Page Generating
	
	procedure Refresh_Page(
		Object : in Renderer; 
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		URI : in String) is
	begin
		Write(Output, 
			"<meta http-equiv=""REFRESH"" content=""0;URL=" & URI & """ />" &
			"<style>body{background-color:black;}</style>");
	end Refresh_Page;
	
	procedure Index_Page(
		Object : in Renderer; 
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Village_List : in Villages.Lists.Village_Lists.Vector; 
		Muramura : Natural;
		User_Id: in String;
		User_Password : in String)
	is
		use Villages.Lists.Village_Lists;
		use type Villages.Village_State;
		procedure Handle(Output : not null access Ada.Streams.Root_Stream_Type'Class;
			Tag : in String; Template : in Ase.Web.Producers.Template) is
		begin
			if Tag = "userpanel" then
				User_Panel(Renderer'Class(Object), Output, Template, User_Id, User_Password, True);
			elsif Tag = "muramura" then
				for I in 1 .. Muramura loop
					Ase.Web.Producers.Produce(Output, Template);
				end loop;
			else
				Handle_List(Output, Tag, Template, Object, Village_List, 10,
					User_Id => User_Id, User_Password => User_Password);
			end if;
		end Handle;
		File: Ada.Streams.Stream_IO.File_Type;
	begin
		Ada.Streams.Stream_IO.Open(File, Ada.Streams.Stream_IO.In_File, 
			Object.Configuration.Template_Index_File_Name.all);
		begin
			Ase.Web.Producers.Produce(Output, 
				Ase.Web.Producers.Read(Ada.Streams.Stream_IO.Stream(File), Natural(Ada.Streams.Stream_IO.Size(File))), 
				Handler => Handle'Access);
		exception
			when others =>
				Ada.Streams.Stream_IO.Close(File);
				raise;
		end;
		Ada.Streams.Stream_IO.Close(File);
	end Index_Page;
	
	procedure List_Page(
		Object : in Renderer; 
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Village_List : in Villages.Lists.Village_Lists.Vector)
	is
		use Villages.Lists.Village_Lists;
		use type Villages.Village_State;
		File: Ada.Streams.Stream_IO.File_Type;
		procedure Handle(Output : not null access Ada.Streams.Root_Stream_Type'Class;
			Tag : in String; Template : in Ase.Web.Producers.Template) is
		begin
			Handle_List(Output, Tag, Template, Object, Village_List, 9999, "", "");
		end Handle;
	begin
		Ada.Streams.Stream_IO.Open(File, Ada.Streams.Stream_IO.In_File, 
			Object.Configuration.Template_List_File_Name.all);
		begin
			Ase.Web.Producers.Produce(Output, 
				Ase.Web.Producers.Read(Ada.Streams.Stream_IO.Stream(File), Natural(Ada.Streams.Stream_IO.Size(File))), 
				Handler => Handle'Access);
		exception
			when others =>
				Ada.Streams.Stream_IO.Close(File);
				raise;
		end;
		Ada.Streams.Stream_IO.Close(File);
	end List_Page;
	
	procedure Register_Page(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Village_Id : in Villages.Lists.Village_Id := Villages.Lists.Invalid_Village_Id;
		New_User_Id : String;
		New_User_Password : String) 
	is
		procedure Handle(Output : not null access Ada.Streams.Root_Stream_Type'Class;
			Tag : in String; Template : in Ase.Web.Producers.Template) is
		begin
			if Tag = "id" then
				Write(Output, New_User_Id);
			elsif Tag = "newid" then
				Write(Output, '"');
				Write(Output, New_User_Id);
				Write(Output, '"');
			elsif Tag = "newpassword" then
				Write(Output, '"');
				Write(Output, Ase.Web.Markup_Entity(New_User_Password));
				Write(Output, '"');
			else
				Handle_Users(Output, Tag, Template, Object, 
					Village_Id => Village_Id, User_Id => "", User_Password => "");
			end if;
		end Handle;
		File: Ada.Streams.Stream_IO.File_Type;
	begin
		Ada.Streams.Stream_IO.Open(File, Ada.Streams.Stream_IO.In_File, 
			Object.Configuration.Template_Register_File_Name.all);
		begin
			Ase.Web.Producers.Produce(Output, 
				Ase.Web.Producers.Read(Ada.Streams.Stream_IO.Stream(File), Natural(Ada.Streams.Stream_IO.Size(File))), 
				Handler => Handle'Access);
		exception
			when others =>
				Ada.Streams.Stream_IO.Close(File);
				raise;
		end;
		Ada.Streams.Stream_IO.Close(File);
	end Register_Page;
	
	procedure User_Page(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Village_List : Villages.Lists.Village_Lists.Vector;
		User_Id : in String;
		User_Password : in String;
		User_Info : in Users.User_Info) 
	is
		Joined : Ada.Strings.Unbounded.Unbounded_String;
		Created : Ada.Strings.Unbounded.Unbounded_String;
		procedure Handle(Output : not null access Ada.Streams.Root_Stream_Type'Class;
			Tag : in String; Template : in Ase.Web.Producers.Template) is
		begin
			if Tag = "userpanel" then
				User_Panel(Renderer'Class(Object), Output, Template, User_Id, User_Password, False);
			elsif Tag = "back" then
				Write(Output, "<a href=");
				Link(Renderer'Class(Object), Output, User_Id => User_Id, User_Password => User_Password);
				Write(Output, '>');
				Ase.Web.Producers.Produce(Output, Template);
				Write(Output, "</a>");
			elsif Tag = "joined" then
				if Joined /= Ada.Strings.Unbounded.Null_Unbounded_String then
					Ase.Web.Producers.Produce(Output, Template, Handler => Handle'Access);
				end if;
			elsif Tag = "nojoined" then
				if Joined = Ada.Strings.Unbounded.Null_Unbounded_String then
					Ase.Web.Producers.Produce(Output, Template);
				end if;
			elsif Tag = "created" then
				if Created /= Ada.Strings.Unbounded.Null_Unbounded_String then
					Ase.Web.Producers.Produce(Output, Template, Handler => Handle'Access);
				end if;
			elsif Tag = "creatable" then
				if Joined = Ada.Strings.Unbounded.Null_Unbounded_String 
					and then Created = Ada.Strings.Unbounded.Null_Unbounded_String 
				then
					Ase.Web.Producers.Produce(Output, Template, Handler => Handle'Access);
				end if;
			elsif Tag = "activevillage" then
				Write(Output, Ase.Web.Markup_HTML(+Joined, Version => Ase.Web.XHTML));
			elsif Tag = "createdvillage" then
				Write(Output, Ase.Web.Markup_HTML(+Created, Version => Ase.Web.XHTML));
			else
				Handle_Users(Output, Tag, Template, Object, 
					User_Id => User_Id, User_Password => User_Password);
			end if;
		end Handle;
		use Villages.Lists.Village_Lists;
		use Ase.Strings.Lists;
		use Ada.Strings.Unbounded;
		use type Villages.Village_State;
		File: Ada.Streams.Stream_IO.File_Type;
	begin
		Ada.Streams.Stream_IO.Open(File, Ada.Streams.Stream_IO.In_File, 
			Object.Configuration.Template_User_File_Name.all);
		begin
			for I in Village_List.First_Index .. Village_List.Last_Index loop
				declare
					V : Villages.Lists.Village_List_Item renames Element(Village_List, I);
					J : Ase.Strings.Lists.Cursor;
				begin
					if V.State < Villages.Epilogue then
						J := V.People.First;
						while Has_Element(J) loop
							if Element(J) = User_Id then
								if Joined /= Ada.Strings.Unbounded.Null_Unbounded_String then
									Append(Joined, "、");
								end if;
								Append(Joined, V.Name);
							end if;
							Next(J);
						end loop;
						if V.By = User_Id then
							Created := V.Name;
						end if;
					end if;
				end;
			end loop;
			Ase.Web.Producers.Produce(Output, 
				Ase.Web.Producers.Read(Ada.Streams.Stream_IO.Stream(File), Natural(Ada.Streams.Stream_IO.Size(File))), 
				Handler => Handle'Access);
		exception
			when others =>
				Ada.Streams.Stream_IO.Close(File);
				raise;
		end;
		Ada.Streams.Stream_IO.Close(File);
	end User_Page;
	
	procedure Village_Page(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Village_Id : Villages.Lists.Village_Id; 
		Village : Villages.Village_Type; 
		Day : Natural;
		First, Last : Integer := -1;
		User_Id : String;
		User_Password : String) 
	is
		use Villages.Messages;
		use type Villages.Person_Array_Access;
		use type Villages.Village_State;
		use type Villages.Village_Time;
		use type Villages.Person_State;
		use type Villages.Message_Kind;
		use type Villages.Daytime_Preview_Mode;
		use type Villages.Person_Role;
		use type Ada.Containers.Count_Type;
		use type Ada.Calendar.Time;
		use type Ase.Web.HTML_Version;
		
		Target_Day : Natural;
		
		procedure Vote_Form(Name : String; Kind : Villages.Person_Role; Player : Integer;
			Current : Integer; Current_Special : Boolean; 
			Message : String; Button : String)
		is
			Including : Boolean;
		begin
			if HTML_Version(Renderer'Class(Object)) = Ase.Web.XHTML then
				Write(Output, "<form method=""POST"" class=""inner"">" & Line_Break &
					"<table><tr>" & Line_Break & "<td class=""input"">");
			else
				Write(Output, "<form method=""POST"" action=");
				Link(Renderer'Class(Object), Output, Village_Id => Village_Id,
					User_Id => User_Id, User_Password => User_Password);
				Write(Output, ">");
			end if;
			Write(Output, Message);
			Write(Output, " <select name=""target"">" & Line_Break);
			for Position in Village.People'Range loop
				if Position /= Player then 
					case Kind is
						when Villages.Detective =>
							Including := Village.People(Position).Records(Target_Day).State = Villages.Died;
						when Villages.Vampire_Role =>
							Including := Village.People(Position).Records(Target_Day).State /= Villages.Died
								and then Village.People(Position).Role not in Villages.Vampire_Role;
						when others =>
							Including := Village.People(Position).Records(Target_Day).State /= Villages.Died;
					end case;
					if Including then
						Write(Output, "<option value=""" & To_String(Position) & """");
						if Current = Position then
							Write(Output, " selected=""selected""");
						end if;
						Write(Output, ">");
						Write(Output, Renderers.Name(Village.People(Position)));
						if Current = Position then
							Write(Output, " *");
						end if;
						Write(Output, "</option>");
					end if;
				end if;
			end loop;
			Write(Output, "<option value=""-1""");
			if Current < 0 then
				Write(Output, " selected=""selected""");
			end if;
			Write(Output, ">棄権");
			if Current < 0 then
				Write(Output, " *");
			end if;
			Write(Output, "</option>" & Line_Break & "</select>" & Line_Break);
			if Kind = Villages.Hunter then
				Write(Output, " <input name=""special"" type=""checkbox"" ");
				if Current_Special then
					Write(Output, "checked=""checked"" ");
				end if;
				Write(Output, "/>銀の弾丸");
			end if;
			if HTML_Version(Renderer'Class(Object)) = Ase.Web.XHTML then
				Write(Output, "</td>" & Line_Break & "<td class=""button"">");
			end if;
			Write(Output, "<input type=""submit"" value=""" & Button & """ />");
			if HTML_Version(Renderer'Class(Object)) = Ase.Web.XHTML then
				Write(Output, "</td>" & Line_Break & "</tr></table>" & Line_Break);
			end if;
			Write(Output,
				"<input type=""hidden"" name=""cmd"" value=""" & Name & """ />" & Line_Break &
				"</form>" & Line_Break);
		end Vote_Form;
		function Role_Text(Person : Villages.Person_Type) return String is
			Setting : Villages.Person_Record renames Person.Records(Village.Today);
		begin
			if Setting.State = Villages.Died then
				return "あなたは幽霊です。";
			else
				case Person.Role is
					when Villages.Inhabitant | Villages.Loved_Inhabitant | Villages.Unfortunate_Inhabitant => 
						return "あなたは村人です。";
					when Villages.Doctor => 
						if Setting.Target >= 0 then
							return "あなたは医者、" & Name(Village.People(Setting.Target)) & "を診察しました。";
						else
							return "あなたは医者です。";
						end if;
					when Villages.Detective => 
						if Setting.Target >= 0 then
							return "あなたは探偵、" & Name(Village.People(Setting.Target)) & "を調査中です。";
						else
							return "あなたは探偵です。";
						end if;
					when Villages.Astronomer => 
						if Person.Commited and Setting.Target >= 0 then
							return "あなたは天文家、" & Name(Village.People(Setting.Target)) & "の家の空を観測します。";
						else
							return "あなたは天文家です。";
						end if;
					when Villages.Hunter => 
						if Person.Commited and Setting.Target >= 0 and Setting.Special then
							return "あなたは猟師、" & Name(Village.People(Setting.Target)) & "を銀の弾丸で守ります。";
						elsif Person.Commited and Setting.Target >= 0 then
							return "あなたは猟師、" & Name(Village.People(Setting.Target)) & "を守ります。";
						elsif Person.Commited and Setting.Special then
							return "あなたは猟師、銃には銀の弾丸を装填しています。";
						else
							return "あなたは猟師です。";
						end if;
					when Villages.Lover =>
						for Position in Village.People'Range loop
							if Village.People(Position).Role = Villages.Loved_Inhabitant then
								return "あなたは" & Name(Village.People(Position)) & "に片想いです。";
							end if;
						end loop;
						pragma Assert(False);
						return "";
					when Villages.Sweetheart_M | Villages.Sweetheart_F =>
						for Position in Village.People'Range loop
							if Village.People(Position).Role /= Person.Role 
								and then Village.People(Position).Role in Villages.Sweetheart_M .. Villages.Sweetheart_F 
							then
								return "あなたは" & Name(Village.People(Position)) & "の恋人です。";
							end if;
						end loop;
						pragma Assert(False);
						return "";
					when Villages.Servant =>
						return "あなたは吸血鬼に身を捧げることが喜びである使徒です。";
					when Villages.Vampire_Role =>
						declare
							Mark : constant array(Villages.Vampire_Role) of Character := ('K', 'Q', 'J');
						begin
							if Person.Commited and Setting.Target >= 0 then
								return "あなたは吸血鬼(" & Mark(Person.Role) & ")、" & Name(Village.People(Setting.Target)) & "を襲います。";
							else
								return "あなたは吸血鬼(" & Mark(Person.Role) & ")です。";
							end if;
						end;
					when Villages.Gremlin =>
						return "あなたは妖魔です。";
					when Villages.Werewolf | Villages.Possessed =>
						raise Program_Error;
				end case;
			end if;
		end Role_Text;
		Player_Index : constant Integer := Villages.Joined(Village, User_Id);
		Message_Counts : Villages.Message_Counts renames Villages.Count_Messages(Village, Day);
		Tip_Showed : Boolean := False;
		type Paging_Pos is (Top, Bottom, Tip);
		procedure Paging(Pos : Paging_Pos) is
			Speech_Count : constant Natural := Villages.Count_Speech(Village, Day);
			F, L, R : Natural;
		begin
			if Pos /= Tip then
				if First < 0 then
					F := 0;
				else
					F := First;
				end if;
				if Last < 0 then
					L := Speech_Count;
				else
					L := Last;
				end if;
				if F = 0 and then L >= Speech_Count then
					Write(Output, "<hr><div>全");
				else
					Write(Output, "<hr><div><a href=");
					Link(Renderer'Class(Object), Output, Village_Id, Day, First => 0, Last => Speech_Count, 
						User_Id => User_Id, User_Password => User_Password);
					Write(Output, ">全</a>");
				end if;
				for I in 0 .. (Speech_Count - 1) / Speeches_By_Page loop
					declare
						I_S : String renames To_String(I + 1);
						I_F : constant Natural := I * Speeches_By_Page;
						I_L : Natural := I_F + (Speeches_By_Page - 1);
					begin
						if Village.State = Villages.Closed or else Day /= Village.Today then
							if I_L > Speech_Count then
								I_L := Speech_Count;
							end if;
						end if;
						if F = I_F and then L = I_L then
							Write(Output, "|" & I_S);
						else
							Write(Output, "|<a href=");
							Link(Renderer'Class(Object), Output, Village_Id, Day, First => I_F, Last => I_L, 
								User_Id => User_Id, User_Password => User_Password);
							Write(Output, '>');
							Write(Output, I_S);
							Write(Output, "</a>");
						end if;
					end;
				end loop;
				if Speech_Count > Speeches_By_Page then
					R := Speech_Count - Speeches_By_Page;
				else
					R := 0;
				end if;
				if Village.State = Villages.Closed or else Day /= Village.Today then
					Write(Output, "|");
				elsif F = R and then L = Speech_Count then
					Write(Output, "|新|");
				else
					Write(Output, "|<a href=");
					Link(Renderer'Class(Object), Output, Village_Id, Day, Latest => Speeches_By_Page,
						User_Id => User_Id, User_Password => User_Password);
					Write(Output, ">新</a>|");
				end if;
			else
				Write(Output, "<hr><div>末端です……");
			end if;
			if Pos = Top then
				Write(Output, "<a name=""top"" href=""#bottom"">下</a></div>");
			else
				Write(Output, "<a name=""bottom"" href=""#top"">上</a></div>");
			end if;
		end Paging;
		procedure Handle(Output : not null access Ada.Streams.Root_Stream_Type'Class;
			Tag : in String; Template : in Ase.Web.Producers.Template) is
		begin
			if Tag = "userpanel" then
				User_Panel(Renderer'Class(Object), Output, Template, User_Id, User_Password, False);
			elsif Tag = "stylesheet" then
				Write(Output, "<link rel=""stylesheet"" type=""text/css"" href=");
				Link_Style_Sheet(Renderer'Class(Object), Output);
				Write(Output, "/>");
			elsif Tag = "background" then
				Link_Image(Renderer'Class(Object), Output, Object.Configuration.Background_Image_File_Name.all);
			elsif Tag = "styles" then
				if Village.People /= null then
					Write(Output, "<style>");
					for I in Village.People'Range loop
						Write(Output, ".p" & To_String(I) & "{display:none;} ");
					end loop;
					Write(Output, "</style>");
					for I in Village.People'Range loop
						Write(Output, "<style id=""s" & To_String(I) & """>.p" & To_String(I) & "{display:block;} </style>");
					end loop;
				end if;
				-- ここでやるべきでもないがついでに
				if Village.State = Villages.Opened
					and then Village.Day_Duration < 24 * 60 * 60.0 
					and then HTML_Version(Renderer'Class(Object)) = Ase.Web.XHTML
				then
					Next_Dawn : declare
						Next : Ada.Calendar.Time;
						Y : Ada.Calendar.Year_Number;
						M : Ada.Calendar.Month_Number;
						D : Ada.Calendar.Day_Number;
						H : Ada.Calendar.Formatting.Hour_Number;
						N : Ada.Calendar.Formatting.Minute_Number;
						S : Ada.Calendar.Formatting.Second_Number;
						T : Ada.Calendar.Formatting.Second_Duration;
					begin
						case Village.Time is
							when Villages.Night => Next := Village.Dawn + Village.Night_Duration;
							when Villages.Daytime => Next := Village.Dawn + (Village.Night_Duration + Village.Day_Duration);
							when Villages.Vote => Next := Village.Dawn + (Village.Night_Duration + Village.Day_Duration + Vote_Duration);
						end case;
						Ada.Calendar.Formatting.Split(Next, Y, M, D, H, N, S, T, Time_Zone => Calendar.Time_Offset);
						declare
							Y : String renames Ada.Calendar.Year_Number'Image(Next_Dawn.Y);
							M : String renames Ada.Calendar.Month_Number'Image(Next_Dawn.M);
							D : String renames Ada.Calendar.Day_Number'Image(Next_Dawn.D);
							H : String renames Ada.Calendar.Formatting.Hour_Number'Image(Next_Dawn.H);
							N : String renames Ada.Calendar.Formatting.Minute_Number'Image(Next_Dawn.N);
							S : String renames Ada.Calendar.Formatting.Second_Number'Image(Next_Dawn.S);
						begin
							Write(Output, Line_Break &
								"<script type=""text/javascript"">" &
								"var limit = new Date(" & Y & "," & M & " - 1," & D & "," & H & "," & N & "," & S & ");" &
								"var timer = setTimeout('on_time()', 500);" &
								"function on_time(){" &
								"clearTimeout(timer);" &
								"var t = (limit.getTime() - (new Date).getTime()) / 1000;" &
								"var m = document.getElementById('min');" &
								"if(m) m.firstChild.nodeValue = Math.floor(t / 60);" &
								"var s = document.getElementById('sec');" &
								"if(s) s.firstChild.nodeValue = Math.floor(t % 60);" &
								"timer = setTimeout('on_time()', 500);" &
								"}" & 
								"</script>");
						end;
					end Next_Dawn;
				end if;
			elsif Tag = "days" then
				for I in 0 .. Village.Today loop
					declare
						procedure Handle_Days(Output : not null access Ada.Streams.Root_Stream_Type'Class;
							Tag : in String; Template : in Ase.Web.Producers.Template) is
						begin
							if Tag = "day" then
								if I /= Day then
									Write(Output, "<a href=");
									Link(Renderer'Class(Object), Output, Village_Id, I, 
										User_Id => User_Id, User_Password => User_Password);
									Write(Output, '>');
								end if;
								Day_Name(Renderer'Class(Object), Output, I, Village.Today, Village.State);
								if I /= Day then
									Write(Output, "</a>");
								end if;
							else
								Handle_Villages(Output, Tag, Template, Object,
									Village_Id, Village, Day, User_Id => User_Id, User_Password => User_Password);
							end if;
						end Handle_Days;
					begin
						Ase.Web.Producers.Produce(Output, Template, Handler => Handle_Days'Access);
					end;
				end loop;
			elsif Tag = "person" then
				if Village.People /= null then
					for I in Village.People'Range loop
						declare
							Person : Villages.Person_Type renames Village.People(I);
							procedure Handle_Summary(Output : not null access Ada.Streams.Root_Stream_Type'Class;
								Tag : in String; Template : in Ase.Web.Producers.Template) is
							begin
								if Tag = "name" then
									Write(Output, 
										"<label for=""c" & To_String(I) & """>" & 
										"<input id=""c" & To_String(I) & """ type=""checkbox"" checked=""checked"" onClick=""javascript:sync(" & To_String(I) & ")"" />" &
										Ase.Web.Markup_Entity(Name(Person)) &
										"</label>");
								elsif Tag = "speech" then
									Write(Output, To_String(Message_Counts(I).Speech));
									if Message_Counts(I).Encouraged > 0 then
										Write(Output, " <small>/");
										Write(Output, To_String(Speech_Limit + Message_Counts(I).Encouraged * Encouraged_Speech_Limit));
										Write(Output, "</small>");
									end if;
								elsif Tag = "administrator" then
									if User_id = Tabula.Users.Administrator then
										Ase.Web.Producers.Produce(Output, Template, Handler => Handle_Summary'Access);
									end if;
								elsif Tag = "id" then
									Write(Output, Ase.Web.Markup_Entity(+Person.Id));
								elsif Tag = "remove" then
									if Village.State = Villages.Prologue then
										Ase.Web.Producers.Produce(Output, Template, Handler => Handle_Summary'Access);
									end if;
								elsif Tag = "htarget" then
									Write(Output, "<input type=""hidden"" name=""target"" value=""");
									Write(Output, To_String(I));
									Write(Output, """/>");
								else
									Handle_Villages(Output, Tag, Template, Object,
										Village_Id, Village, Day, User_Id => User_Id, User_Password => User_Password);
								end if;
							end Handle_Summary;
						begin
							if (Village.State >= Villages.Epilogue and then Village.Today = Day) 
								or else Person.Records(Day).State /= Villages.Died
							then
								Ase.Web.Producers.Produce(Output, Template, Handler => Handle_Summary'Access);
							end if;
						end;
					end loop;
				end if;
			elsif Tag = "back" then
				Write(Output, "<a href=");
				Link(Renderer'Class(Object), Output, User_Id => User_Id, User_Password => User_Password);
				Write(Output, '>');
				Ase.Web.Producers.Produce(Output, Template);
				Write(Output, "</a>");
			elsif Tag = "message" then
				declare
					procedure Narration(Message : String; Class : String := "narration") is
						procedure Handle_Narration(Output : not null access Ada.Streams.Root_Stream_Type'Class;
							Tag : in String; Template : in Ase.Web.Producers.Template) is
						begin
							Write(Output, Ase.Web.Markup_HTML(Message, HTML_Version(Renderer'Class(Object))));
						end Handle_Narration;
					begin
						Ase.Web.Producers.Produce(Output, Template, Class, Handler => Handle_Narration'Access);
					end Narration;
					procedure Speech(Message : Villages.Message; Class : String; Time : Ada.Calendar.Time) is
						procedure Handle_Speech(Output : not null access Ada.Streams.Root_Stream_Type'Class;
							Tag : in String; Template : in Ase.Web.Producers.Template) is
						begin
							Handle_Messages(Output, Tag, Template, Object,
								Village_Id, Village, Day, Message, Time, User_Id => User_Id, User_Password => User_Password);
						end Handle_Speech;
					begin
						Ase.Web.Producers.Produce(Output, Template, Class, Handler => Handle_Speech'Access);
					end Speech;
					procedure Note(Subject : Villages.Person_Type; Note : Villages.Person_Record; Class : String) is
						procedure Handle_Note(Output : not null access Ada.Streams.Root_Stream_Type'Class;
							Tag : in String; Template : in Ase.Web.Producers.Template) is
						begin
							if Tag = "image" then
								Link_Image(Renderer'Class(Object), Output, +Subject.Image);
							elsif Tag = "name" then
								Write(Output, Name(Subject));
							elsif Tag = "text" then
								declare
									S : String renames Ada.Strings.Unbounded.To_String(Note.Note);
								begin
									if S = "" then
										Write(Output, "……。");
									else
										Write(Output, Ase.Web.Markup_HTML(S, HTML_Version(Object)));
									end if;
								end;
							else
								Handle_Villages(Output, Tag, Template, Object, 
									Village_Id, Village, Day, User_Id => User_Id, User_Password => User_Password);
							end if;
						end Handle_Note;
					begin
						Ase.Web.Producers.Produce(Output, Template, Class, Handler => Handle_Note'Access);
					end Note;
					subtype X_Type is Integer range 1 .. 3;
					package Random_X is new Ase.Numerics.MT19937.Discrete_Random(X_Type);
					Position : Villages.Messages.Cursor := Villages.Messages.First(Village.Messages);
					Executed : Integer := -1;
					Speech_Count : Natural := 0;
					X : X_Type := 2;
					Last_Speech : Integer := -1;
					Last_Speech_Time : Ada.Calendar.Time := Calendar.Null_Time;
					X_Generator : Ase.Numerics.MT19937.Generator;
				begin
					Ase.Numerics.MT19937.Reset(X_Generator, 12);
					if HTML_Version(Renderer'Class(Object)) = Ase.Web.HTML then
						Paging(Top);
					end if;
					while Has_Element(Position) loop
						declare
							Message : Villages.Message renames Element(Position);
						begin
							if Message.Day = Day then
								if (First < 0 or else First <= Speech_Count) and then (Last < 0 or else Speech_Count <= Last) then
									case Message.Kind is
										when Villages.Narration =>
											Narration(+Message.Text);
										when Villages.Escape =>
											declare
												Subject : Villages.Person_Type renames Village.Escaped_People(Message.Subject);
											begin
												if Village.State >= Villages.Epilogue then
													Narration(Name(Subject) & "(" & (+Subject.Id) & ")は人知れず華やいだ都会へと旅立ってゆきました。", Class => "narratione");
												else
													Narration(Name(Subject) & "は人知れず華やいだ都会へと旅立ってゆきました。", Class => "narratione");
												end if;
											end;
										when Villages.Join =>
											declare
												Subject : Villages.Person_Type renames Village.People(Message.Subject);
											begin
												Narration(To_String(Message.Subject + 1) & "人目に" & Name(Subject) & "が現れました。");
											end;
										when Villages.Escaped_Join =>
											declare
												Subject : Villages.Person_Type renames Village.Escaped_People(Message.Subject);
											begin
												Narration(Name(Subject) & "が現れました。", Class => "narratione");
											end;
										when Villages.Speech | Villages.Escaped_Speech =>
											if Message.Kind = Villages.Speech 
												or else Villages.Rejoined(Village, Message.Subject) >= 0
											then
												declare
													Subject : Integer;
												begin
													if Message.Kind = Villages.Speech then
														Subject := Message.Subject;
													else
														Subject := Villages.Rejoined(Village, Message.Subject);
													end if;
													if Last_Speech /= Subject then
														New_X : loop
															declare
																X2 : constant X_Type := Random_X.Random(X_Generator);
															begin
																if X2 /= X then
																	X := X2;
																	exit New_X;
																end if;
															end;
														end loop New_X;
														Last_Speech := Subject;
														if Message.Time - Last_Speech_Time < Speech_Simultaneous then
															declare
																Uso : constant Duration := Duration(Long_Long_Integer(Message.Time - Calendar.Null_Time) rem 3);
															begin
																Last_Speech_Time := Last_Speech_Time + Speech_Simultaneous + Uso;
															end;
														else
															Last_Speech_Time := Message.Time;
														end if;
													else
														Last_Speech_Time := Message.Time;
													end if;
												end;
												if HTML_Version(Renderer'Class(Object)) = Ase.Web.XHTML then
													Write(Output, "<div class=""s");
													Write(Output, To_String(X));
													Write(Output, " p");
													Write(Output, To_String(Last_Speech));
													Write(Output, """>");
												end if;
												Speech(Message, "speech", Last_Speech_Time);
												if HTML_Version(Renderer'Class(Object)) = Ase.Web.XHTML then
													Write(Output, "</div>");
												end if;
											else
												Speech(Message, "escaped", Message.Time);
											end if;
										when Villages.Monologue =>
											if Village.State >= Villages.Epilogue 
												or else Message.Subject = Player_Index
											then
												Speech(Message, "monologue", Message.Time);
											end if;
										when Villages.Ghost =>
											if Village.State >= Villages.Epilogue 
												or else (Player_Index >= 0 and then Village.People(Player_Index).Records(Village.Today).State = Villages.Died)
											then
												Speech(Message, "ghost", Message.Time);
											end if;
										when Villages.Howling =>
											if Village.State >= Villages.Epilogue 
												or else (Player_Index >= 0 and then Village.People(Player_Index).Role in Villages.Vampire_Role)
											then
												Speech(Message, "vampire", Message.Time);
											end if;
										when Villages.Howling_Blocked =>
											if Village.State >= Villages.Epilogue 
												or else (Player_Index >= 0 and then Village.People(Player_Index).Role in Villages.Vampire_Role)
											then
												declare
													The_Unfortunate : constant Integer := Village.Find_Superman(Villages.Unfortunate_Inhabitant);
												begin
													Narration(Name(Village.People(The_Unfortunate)) & "のせいで用事ができてしまい、今夜は相談ができません。", "narrationi");
												end;
											end if;
										when Villages.Action_Message_Kind =>
											declare
												Subject : Villages.Person_Type renames Village.People(Message.Subject);
												Target : Villages.Person_Type renames Village.People(Message.Target);
											begin
												case Villages.Action_Message_Kind(Message.Kind) is
													when Villages.Action_Wake =>
														Narration(Name(Subject) & "は" & Name(Target) & "を起こした。");
													when Villages.Action_Encourage =>
														Narration(Name(Subject) & "は" & Name(Target) & "に話の続きを促した。");
													when Villages.Action_Vampire_Gaze =>
														if Village.State >= Villages.Epilogue 
															or else (Player_Index >= 0 and then Village.People(Player_Index).Role in Villages.Vampire_Role)
														then
															Narration(Name(Subject) & "は" & Name(Target) & "をこっそりと見つめた。", "narrationi");
														end if;
													when Villages.Action_Vampire_Gaze_Blocked =>
														if Village.State >= Villages.Epilogue 
															or else (Player_Index >= 0 and then Village.People(Player_Index).Role in Villages.Vampire_Role)
														then
															declare
																The_Unfortunate : constant Integer := Village.Find_Superman(Villages.Unfortunate_Inhabitant);
															begin
																Narration(Name(Subject) & "の視線は" & Name(Village.People(The_Unfortunate)) & "に遮られた。", "narrationi");
															end;
														end if;
												end case;
											end;
										when Villages.Servant_Message_Kind =>
											if Village.State >= Villages.Epilogue or else Player_Index = Message.Subject then
												Narration(Servant_Knew_Message(Village, Message), "narrationi");
											end if;
										when Villages.Doctor_Message_Kind =>
											if Village.State >= Villages.Epilogue or else (Player_Index = Message.Subject) then
												Narration(Doctor_Cure_Message(Village, Message), "narrationi");
											end if;
										when Villages.Detective_Message_Kind =>
											if Village.State >= Villages.Epilogue or else (Player_Index = Message.Subject) then
												Narration(Detective_Survey_Message(Village, Message), "narrationi");
												if Message.Text /= Ada.Strings.Unbounded.Null_Unbounded_String 
													and then (
														Village.Daytime_Preview = Villages.Role_And_Message 
														or else Village.Daytime_Preview = Villages.Message_Only
														or else Message.Kind /= Villages.Detective_Survey_Preview)
												then
													Speech(Message, "dying", Message.Time);
												end if;
											end if;
										when Villages.Execution =>
											declare
												use Ada.Strings.Unbounded;
												Text : Ada.Strings.Unbounded.Unbounded_String;
												Voted : array(Village.People'Range) of Natural := (others => 0);
												Max_Voted : Natural := 0;
												Execution_Day : constant Natural := Message.Day - 1;
											begin
												for I in Voted'Range loop
													declare
														P : Villages.Person_Type renames Village.People(I);
														V : constant Integer := P.Records(Execution_Day).Vote;
													begin
														if V >= 0 then
															Voted(V) := Voted(V) + 1;
															if Voted(V) > Max_Voted then
																Max_Voted := Voted(V);
															end if;
															if Village.State >= Villages.Epilogue or Player_Index = I then
																declare
																	T : Villages.Person_Type renames Village.People(V);
																begin
																	Append(Text, Name(P) & "は" & Name(T) & "に投票しました。" & Line_Break);
																end;
															end if;
														end if;
													end;
												end loop;
												if Text /= Ada.Strings.Unbounded.Null_Unbounded_String then
													Narration(+Text, "narrationi");
												end if;
												Text := Ada.Strings.Unbounded.Null_Unbounded_String;
												for Count in reverse 1 .. Max_Voted loop
													for I in Voted'Range loop
														if Voted(I) = Count then
															declare
																P : Villages.Person_Type renames Village.People(I);
															begin
																Append(Text, To_String(Voted(I)) & "票が" & 
																	Name(P) & "に集まりました。" & Line_Break);
															end;
														end if;
													end loop;
												end loop;
												declare
													E : Villages.Person_Type renames Village.People(Message.Target);
												begin
													Append(Text, Name(E) & "は心臓に杭を打ち込まれました。");
												end;
												Narration(+Text);
											end;
											Executed := Message.Target;
										when Villages.Awareness =>
											if Village.State >= Villages.Epilogue or else Player_Index = Message.Subject then
												declare
													Subject : Villages.Person_Type renames Village.People(Message.Subject);
												begin
													Narration(Name(Subject) & "は無性に闇が恋しくなり……夜空へと飛び立ちました。", "narrationi");
												end;
											end if;
										when Villages.Astronomer_Observation =>
											if Village.State >= Villages.Epilogue or else Player_Index = Message.Subject then
												Narration(Astronomer_Observation_Message(Village, Message), "narrationi");
											end if;
										when Villages.Hunter_Message_Kind =>
											if Village.State >= Villages.Epilogue or else Player_Index = Message.Subject then
												Narration(Hunter_Guard_Message(Village, Message), "narrationi");
											end if;
										when Villages.Meeting => null;
											if Village.State >= Villages.Epilogue 
												or else (Player_Index >= 0 and then Village.People(Player_Index).Role in Villages.Vampire_Role)
											then
												for Role in Villages.Vampire_Role loop
													for I in Village.People'Range loop
														declare
															Person : Villages.Person_Type renames Village.People(I);
														begin
															if Person.Role = Role then
																declare
																	Yesterday_Record : Villages.Person_Record renames Person.Records(Day - 1);
																begin
																	if Yesterday_Record.State /= Villages.Died and then Executed /= I then
																		Note(Person, Yesterday_Record, "vampire");
																	end if;
																end;
															end if;
														end;
													end loop;
												end loop;
											end if;
										when Villages.Vampire_Message_Kind =>
											if Village.State >= Villages.Epilogue or else (Player_Index >= 0 and then (Message.Subject = Player_Index or else (
												Village.People(Message.Subject).Role in Villages.Vampire_Role
												and then Village.People(Player_Index).Role in Villages.Vampire_Role)))
											then
												Narration(Vampire_Murder_Message(Village, Message, Executed), "narrationi");
											end if;
										when Villages.Gremlin_Sense =>
											if Village.State >= Villages.Epilogue or else (Player_Index = Message.Subject) then
												declare
													Vampire_Count : Natural := 0;
												begin
													for I in Village.People'Range loop
														declare
															P : Villages.Person_Type renames Village.People(I);
															R : Villages.Person_Record renames P.Records(Message.Day);
														begin
															if (R.State = Villages.Normal and then P.Role in Villages.Vampire_Role) 
																or else R.State = Villages.Infected
															then
																Vampire_Count := Vampire_Count + 1;
															end if;
														end;
													end loop;
													Narration("残り吸血鬼の数は" & To_String(Vampire_Count) & "匹……。", "narrationi");
												end;
											end if;
										when Villages.Gremlin_Killed =>
											if Village.State >= Villages.Epilogue or else (Player_Index = Message.Subject) then
												Narration("妖魔は滅びました。", "narrationi");
											end if;
										when Villages.Sweetheart_Incongruity =>
											if Village.State >= Villages.Epilogue or else (Player_Index = Message.Subject) then
												declare
													S : Villages.Person_Type renames Village.People(Message.Subject);
													T : Villages.Person_Type renames Village.People(Message.Target);
												begin
													Narration(Name(S) & "は" & Name(T) & "に違和感を感じました。", "narrationi");
												end;
											end if;
										when Villages.Sweetheart_Suicide =>
											if Village.State >= Villages.Epilogue or else (Player_Index = Message.Subject) or else (Player_Index = Message.Target) then
												declare
													S : Villages.Person_Type renames Village.People(Message.Subject);
												begin
													Narration(Name(S) & "は想い人の後を追いました。", "narrationi");
												end;
											end if;
										when Villages.List =>
											declare
												use Ada.Strings.Unbounded;
												Log : Ada.Strings.Unbounded.Unbounded_String;
											begin
												if Village.Today = Message.Day and Village.State >= Villages.Epilogue then
													declare
														S : String renames Fatalities_List(Village, Message.Day, Executed);
													begin
														if S /= "" then
															Narration(S);
														end if;
													end;
													declare
														Last_Day_Messages : Villages.Message_Counts renames Villages.Count_Messages(Village, Message.Day - 1);
														G_Win : Boolean := False;
														V_Win : Boolean := False;
														Second : Boolean := False;
													begin
														for I in Village.People'Range loop
															declare
																P : Villages.Person_Type renames Village.People(I);
																Speech_Count : constant Natural := Last_Day_Messages(I).Speech;
															begin
																if Second then
																	Append(Log, Line_Break);
																end if;
																Append(Log, Name(P) & "(" & P.Id & ")は" & Image(P.Role) & "でした。");
																case P.Records(Message.Day).State is
																	when Villages.Normal =>
																		Append(Log, "生存しました。");
																		case P.Role is
																			when Villages.Vampire_Role =>
																				if Speech_Count > 0 then
																					V_Win := True;
																				end if;
																			when Villages.Gremlin =>
																				if Speech_Count > 0 then
																					G_Win := True;
																				end if;
																			when others => null;
																		end case;
																		if Speech_Count = 0 then
																			Append(Log, "蚊帳の外でした。");
																		end if;
																	when Villages.Infected =>
																		Append(Log, "吸血鬼にされました。");
																	when Villages.Died =>
																		Append(Log, "死亡しました。");
																end case;
																Second := True;
															end;
														end loop;
														Narration(+Log);
														Log := Null_Unbounded_String;
														if G_Win then
															if V_Win then
																Append(Log, "村は吸血鬼の手に落ちた……流石の吸血鬼も安堵したその油断を突き……妖魔は全てに勝利しました。");
															else
																Append(Log, "吸血鬼を退治した……その瞬間、彼奴を阻むものは何もなくなり……妖魔は全てに勝利しました。");
															end if;
														elsif V_Win then
															Append(Log, "村は吸血鬼の手に落ちました。吸血鬼の勝利です！");
														else
															Append(Log, "吸血鬼を退治しました。村人の勝利です！");
														end if;
													end;
												else
													declare
														S : String renames Fatalities_List(Village, Message.Day, Executed);
													begin
														Append(Log, S);
														if S /= "" then
															Append(Log, Line_Break);
														end if;
													end;
													Append(Log, Survivors_List(Village, Message.Day));
												end if;
												Narration(+Log);
											end;
										when Villages.Introduction =>
											Narration(Stages(Stage(Village)).Introduction.all);
										when Villages.Breakdown => null;
											if Village.State >= Villages.Epilogue 
												or else (Player_Index >= 0 and then Village.People(Player_Index).Role in Villages.Vampire_Role) 
											then
												Narration(Vampires_List(Village), "narrationi");
											end if;
											Narration(Stages(Stage(Village)).Breakdown.all);
											Narration(Breakdown_List(Village));
									end case;
								end if;
								if Message.Kind = Villages.Speech or else Message.Kind = Villages.Escaped_Speech then
									Speech_Count := Speech_Count + 1;
								end if;
							end if;
						end;
						Next(Position);
					end loop;
					if Last < 0 or else Speech_Count <= Last + 1 then
						Tip_Showed := True;
						if Village.State >= Villages.Epilogue and then Day < Village.Today then
							for I in Village.People'Range loop
								declare
									Subject : Villages.Person_Type renames Village.People(I);
									Rec : Villages.Person_Record renames Subject.Records(Day);
								begin
									if Rec.State = Villages.Died and then Rec.Note /= Ada.Strings.Unbounded.Null_Unbounded_String then
										Note(Subject, Rec, "dying");
									end if;
								end;
							end loop;
						end if;
						if Day = Village.Today and then Village.State /= Villages.Closed then
							declare
								procedure Handle_Guidance(Output : not null access Ada.Streams.Root_Stream_Type'Class;
									Tag : in String; Template : in Ase.Web.Producers.Template) is
								begin
									if Village.State <= Villages.Opened then
										declare
											Second : Boolean := False;
										begin
											if Village.People /= null then
												for I in Village.People'Range loop
													declare
														P : Villages.Person_Type renames Village.People(I);
													begin
														if not P.Commited and then P.Records(Village.Today).State /= Villages.Died then
															if Second then
																Write(Output, "、");
															end if;
															Write(Output, Name(P));
															Second := True;
														end if;
													end;
												end loop;
											end if;
											if Second then
												Write(Output, "が行動しています。");
											end if;
										end;
									end if;
									case Village.State is
										when Villages.Prologue =>
											if Village.People = null or else Village.People'Length < Minimum_Number_Of_Persons then
												Write(Output, To_String(Minimum_Number_Of_Persons));
												Write(Output, "人以上の参加を待っています。");
											else
												Write(Output, "全員が行動を終えると夜が明けます。");
											end if;
										when Villages.Opened =>
											case Village.Time is
												when Villages.Daytime => 
													Write(Output, Ada.Calendar.Formatting.Image(Village.Dawn + Village.Day_Duration, Time_Zone => Calendar.Time_Offset));
													Write(Output, "までに行動を終えてください。");
												when Villages.Vote =>
													Write(Output, "全員の投票を待っています。");
												when Villages.Night =>
													Write(Output, "夜です。");
											end case;
											if Village.Day_Duration < 24 * 60 * 60.0 
												and then HTML_Version(Renderer'Class(Object)) = Ase.Web.XHTML
											then
												Write(Output, "あと<span id=""min"">?</span>分<span id=""sec"">?</span>秒です。");
											end if;
										when Villages.Epilogue =>
											declare
												Next_Duration : constant Duration := Duration'Max(
													Village.Day_Duration, Epilogue_Min_Duration);
											begin
												Write(Output, Ada.Calendar.Formatting.Image(Village.Dawn + Next_Duration, Time_Zone => Calendar.Time_Offset));
											end;
											Write(Output, "まで話すことができます。");
										when Villages.Closed => null;
									end case;
								end;
							begin
								Ase.Web.Producers.Produce(Output, Template, "narration", Handler => Handle_Guidance'Access);
							end;
						end if;
						if HTML_Version(Renderer'Class(Object)) = Ase.Web.HTML then
							Paging(Village_Page.Tip);
						end if;
					else
						if HTML_Version(Renderer'Class(Object)) = Ase.Web.HTML then
							Paging(Bottom);
						end if;
					end if;
				end;
			elsif Tag = "villagepanel" then
				if ((Player_Index >= 0 or else User_Id = Tabula.Users.Administrator) and Village.Today = Day) 
					or else (User_Id /= "" and Village.State = Villages.Prologue) 
				then
					if Village.State = Villages.Closed then
						Ase.Web.Producers.Produce(Output, Template, "closed");
					elsif Player_Index >= 0 then
						declare
							Person : Villages.Person_Type renames Village.People(Player_Index);
							Bottom : Boolean := True;
							procedure Handle_Player(Output : not null access Ada.Streams.Root_Stream_Type'Class;
								Tag : in String; Template : in Ase.Web.Producers.Template) is
							begin
								if Tag = "bottom" then
									if Bottom then
										Write(Output, """bottom""");
										Bottom := False;
									else
										Write(Output, """""");
									end if;
								elsif Tag = "name" then
									Write(Output, Name(Person));
								elsif Tag = "speech" then
									if Village.State = Villages.Epilogue or else (
										(Village.State = Villages.Opened or else Village.State = Villages.Prologue)
										and then Village.People(Player_Index).Records(Village.Today).State /= Villages.Died 
										and then not Person.Commited)
									then
										if Village.State = Villages.Opened then
											declare
												Rest : constant Integer := Speech_Limit + Message_Counts(Player_Index).Encouraged * Encouraged_Speech_Limit - Message_Counts(Player_Index).Speech;
												procedure Handle_Speech(Output : not null access Ada.Streams.Root_Stream_Type'Class;
													Tag : in String; Template : in Ase.Web.Producers.Template) is
												begin
													if Tag = "count" then
														Write(Output, To_String(Rest));
													elsif Tag = "rest" then
														Ase.Web.Producers.Produce(Output, Template, Handler => Handle_Speech'Access);
													else
														Handle_Player(Output, Tag, Template);
													end if;
												end Handle_Speech;
											begin
												if Rest > 0 then
													Ase.Web.Producers.Produce(Output, Template, Handler => Handle_Speech'Access);
												end if;
											end;
										else
											Ase.Web.Producers.Produce(Output, Template, Handler => Handle_Player'Access);
										end if;
									end if;
								elsif Tag = "monologue" then
									if Village.State = Villages.Opened
										and then Village.People(Player_Index).Records(Village.Today).State /= Villages.Died 
										and then not Person.Commited 
									then
										declare
											Rest : constant Integer := Monologue_Limit - Message_Counts(Player_Index).Monologue;
											procedure Handle_Monologue(Output : not null access Ada.Streams.Root_Stream_Type'Class;
												Tag : in String; Template : in Ase.Web.Producers.Template) is
											begin
												if Tag = "count" then
													Write(Output, To_String(Rest));
												elsif Tag = "rest" then
													Ase.Web.Producers.Produce(Output, Template, Handler => Handle_Monologue'Access);
												else
													Handle_Player(Output, Tag, Template);
												end if;
											end Handle_Monologue;
										begin
											if Rest > 0 then
												Ase.Web.Producers.Produce(Output, Template, Handler => Handle_Monologue'Access);
											end if;
										end;
									end if;
								elsif Tag ="ghost" then
									if Village.State = Villages.Opened 
										and then Village.People(Player_Index).Records(Village.Today).State = Villages.Died
									then
										declare
											Rest : constant Integer := Ghost_Limit - Message_Counts(Player_Index).Ghost;
											procedure Handle_Ghost(Output : not null access Ada.Streams.Root_Stream_Type'Class;
												Tag : in String; Template : in Ase.Web.Producers.Template) is
											begin
												if Tag = "count" then
													Write(Output, To_String(Rest));
												elsif Tag = "rest" then
													Ase.Web.Producers.Produce(Output, Template, Handler => Handle_Ghost'Access);
												else
													Handle_Player(Output, Tag, Template);
												end if;
											end Handle_Ghost;
										begin
											if Rest > 0 then
												Ase.Web.Producers.Produce(Output, Template, Handler => Handle_Ghost'Access);
											end if;
										end;
									end if;
								elsif Tag ="vampire" then
									if Village.State = Villages.Opened 
										and then not Person.Commited 
										and then Person.Role in Villages.Vampire_Role
										and then (Message_Counts(Player_Index).Speech > 0 or else Village.Time = Villages.Night)
									then
										Ase.Web.Producers.Produce(Output, Template, Handler => Handle_Player'Access);
									end if;
								elsif Tag ="dying" then
									if Village.State = Villages.Opened 
										and then not Person.Commited 
										and then Village.People(Player_Index).Records(Village.Today).State = Villages.Died
									then
										Ase.Web.Producers.Produce(Output, Template, Handler => Handle_Player'Access);
									end if;
								elsif Tag = "note" then
									Write(Output, +Person.Records(Target_Day).Note);
								elsif Tag = "rest" then
									null;
								elsif Tag = "zero" then
									if Village.State = Villages.Opened
										and then Village.Time /= Villages.Night
										and then not Village.People(Player_Index).Commited
										and then Village.People(Player_Index).Records(Village.Today).State /= Villages.Died 
										and then Message_Counts(Player_Index).Speech = 0 
									then
										Ase.Web.Producers.Produce(Output, Template);
									end if;
								elsif Tag = "role" then
									if Village.State = Villages.Opened then
										if Village.People(Player_Index).Records(Village.Today).State /= Villages.Died then
											Ase.Web.Producers.Produce(Output, Template, Handler => Handle_Player'Access);
										else
											Write(Output, Role_Text(Person));
										end if;
									end if;
								elsif Tag = "roletext" then
									Write(Output, Role_Text(Person));
								elsif Tag = "roleimg" then
									if Village.People(Player_Index).Records(Village.Today).State /= Villages.Died then
										Link_Image(Renderer'Class(Object), Output, Role_Image_File_Name(Person.Role).all);
									end if;
								elsif Tag = "vote" then
									if Village.State = Villages.Opened 
										and then Message_Counts(Player_Index).Speech > 0
									then
										if Person.Commited then
											declare
												Setting : Villages.Person_Record renames Person.Records(Village.Today);
											begin
												if Setting.Vote < 0 then
													Write(Output, "<div>処刑を選ぶ投票は棄権します。</div>");
												else
													Write(Output, "<div>処刑には");
													declare
														Target : Villages.Person_Type renames Village.People(Setting.Vote);
													begin
														Write(Output, Name(Target));
													end;
													Write(Output, "を推しています。</div>");
												end if;
											end;
										else
											Vote_Form("vote", Villages.Inhabitant, 
												Player => Player_Index, 
												Current => Person.Records(Village.Today).Vote, 
												Current_Special => False, 
												Message => "処刑を選ばねばなりません……", 
												Button => "投票");
										end if;
									end if;
								elsif Tag = "ability" then
									if Village.State = Villages.Opened 
										and then not Person.Commited
										and then (Message_Counts(Player_Index).Speech > 0 or else Village.Time = Villages.Night)
									then
										case Person.Role is
											when Villages.Inhabitant | Villages.Loved_Inhabitant | Villages.Unfortunate_Inhabitant 
												| Villages.Lover | Villages.Sweetheart_M | Villages.Sweetheart_F 
												| Villages.Servant | Villages.Gremlin => null;
											when Villages.Doctor =>
												if Village.People(Player_Index).Records(Village.Today).Target < 0 then
													if Village.Today >= 2 then
														Vote_Form("target", Villages.Doctor, Player_Index, 
															Person.Records(Village.Today).Target, False, "貴重な薬を誰に……", "診察");
													else
														Write(Output, "<div>今は他に犠牲者がいないと信じましょう。</div>");
													end if;
												end if;
											when Villages.Detective =>
												if Village.People(Player_Index).Records(Village.Today).Target < 0 then
													for I in Village.People'Range loop
														if Village.People(I).Records(Village.Today).State = Villages.Died then
															Vote_Form("target", Villages.Detective, Player_Index, 
																Person.Records(Village.Today).Target, False, "どの被害者を調べますか……", "調査");
															goto Exit_Detective_Target;
														end if;
													end loop;
													if Village.Victim_Existing and Day <= 1 then
														Write(Output, "<div>地主さんを調査しています。</div>");
													else
														Write(Output, "<div>まだ村人に被害者はいません。</div>");
													end if;
													<<Exit_Detective_Target>> null;
												end if;
											when Villages.Astronomer =>
												Vote_Form("target", Villages.Astronomer, Player_Index, 
													Person.Records(Target_Day).Target, False, "どの家の上空の星が奇麗……", "観測");
											when Villages.Hunter =>
												for I in 0 .. Target_Day - 1 loop
													if Person.Records(I).Special then
														Vote_Form("target", Villages.Inhabitant, Player_Index, 
															Person.Records(Target_Day).Target, False, "誰を守りますか……", "護衛");
														goto Exit_Hunter_Target;
													end if;
												end loop;
												Vote_Form("target", Villages.Hunter, Player_Index, 
													Person.Records(Target_Day).Target, Person.Records(Target_Day).Special, "誰を守りますか……", "護衛");
												<<Exit_Hunter_Target>> 
												null;
											when Villages.Vampire_Role =>
												Vote_Form("target", Villages.Vampire_K, Player_Index, 
													Person.Records(Target_Day).Target, False, "誰の血が旨そうでしょうか……", "襲撃");
											when Villages.Werewolf | Villages.Possessed =>
												raise Program_Error;
										end case;
									end if;
								elsif Tag = "action" then
									if Village.State = Villages.Opened and then (
										Message_Counts(Player_Index).Wake = 0 
										or else Message_Counts(Player_Index).Encourage = 0 
										or else ((Village.People(Player_Index).Role in Villages.Vampire_Role) 
											and then Message_Counts(Player_Index).Vampire_Gaze = 0))
									then
										if HTML_Version(Renderer'Class(Object)) = Ase.Web.XHTML then
											Write(Output, "<form method=""POST"" class=""inner"">" & Line_Break);
										else
											Write(Output, "<form method=""POST"" action=");
											Link(Renderer'Class(Object), Output, Village_Id => Village_Id,
												User_Id => User_Id, User_Password => User_Password);
											Write(Output, ">");
										end if;
										Write(Output, "<select name=""target"">" & Line_Break);
										Write(Output, "<option value=""-1"" selected=""selected""></option>" & Line_Break);
										for I in Village.People'Range loop
											if I /= Player_Index 
												and then Village.People(I).Records(Village.Today).State /= Villages.Died 
											then
												declare
													Person : Villages.Person_Type renames Village.People(I);
												begin
													Write(Output, "<option value=""" & To_String(I) & """>");
													Write(Output, Name(Person));
													Write(Output, "</option>");
												end;
											end if;
										end loop;
										Write(Output, "</select>" & Line_Break);
										Write(Output, "<select name=""action"">" & Line_Break);
										Write(Output, "<option value="""" selected=""selected""></option>" & Line_Break);
										if Message_Counts(Player_Index).Wake = 0 then
											Write(Output, "<option value=""wake"">を起こす</option>" & Line_Break);
										end if;
										if Message_Counts(Player_Index).Encourage = 0 then
											Write(Output, "<option value=""encourage"">に話の続きを促す</option>" & Line_Break);
										end if;
										if Village.People(Player_Index).Role in Villages.Vampire_Role
											and then Message_Counts(Player_Index).Vampire_Gaze = 0 
										then
											Write(Output, "<option value=""vampire_gaze"">をこっそり見つめる。</option>" & Line_Break);
										end if;
										Write(Output, "</select>" & Line_Break);
										Write(Output, "<input type=""submit"" value=""行動"" />");
										Write(Output, "<input type=""hidden"" name=""cmd"" value=""action"" />" & Line_Break &
											"</form>" & Line_Break);
									end if;
								elsif Tag = "active" then
									if not Person.Commited 
										and then Village.State /= Villages.Epilogue 
										and then (Message_Counts(Player_Index).Speech > 0 or else Village.State = Villages.Prologue)
									then
										declare
											Setting : Villages.Person_Record renames Person.Records(Village.Today);
										begin
											if Setting.State /= Villages.Died then
												Ase.Web.Producers.Produce(Output, Template, Handler => Handle_Player'Access);
											end if;
										end;
									end if;
								elsif Tag = "escape" then
									if Village.State = Villages.Prologue then
										declare
											subtype Arg is Integer range 1000 .. 4999;
											package Random_Arg is new Ase.Numerics.MT19937.Discrete_Random(Arg);
											Seed : Ase.Numerics.MT19937.Generator;
											X : Arg;
											Y : Arg;
											procedure Handle_Escape(Output : not null access Ada.Streams.Root_Stream_Type'Class;
												Tag : in String; Template : in Ase.Web.Producers.Template) is
											begin
												if Tag = "x" then
													Write(Output, To_String(X));
												elsif Tag = "qx" then
													Write(Output, '"' & To_String(X) & '"');
												elsif Tag = "y" then
													Write(Output, To_String(Y));
												elsif Tag = "qy" then
													Write(Output, '"' & To_String(Y) & '"');
												else
													Handle_Player(Output, Tag, Template);
												end if;
											end Handle_Escape;
										begin
											Ase.Numerics.MT19937.Reset(Seed);
											X := Random_Arg.Random(Seed);
											Y := Random_Arg.Random(Seed);
											Ase.Web.Producers.Produce(Output, Template, Handler => Handle_Escape'Access);
										end;
									end if;
								elsif Tag = "commited" then
									if Person.Commited then
										Ase.Web.Producers.Produce(Output, Template, Handler => Handle_Player'Access);
									end if;
								else
									Handle_Villages(Output, Tag, Template, Object,
										Village_Id, Village, Day, User_Id => User_Id, User_Password => User_Password);
								end if;
							end Handle_Player;
						begin
							Ase.Web.Producers.Produce(Output, Template, "player", Handler => Handle_Player'Access);
						end;
					elsif User_Id = Tabula.Users.Administrator then
						Ase.Web.Producers.Produce(Output, Template, "administrator", Handler=> Handle'Access);
					elsif Village.State > Villages.Prologue then
						Ase.Web.Producers.Produce(Output, Template, "opened");
					elsif Village.People /= null and then Village.People'Length >= Tabula.Maximum_Number_Of_Persons then
						Ase.Web.Producers.Produce(Output, Template, "over");
					else
						declare
							Cast : Villages.Casts.Cast_Type;
							procedure Handle_Entry(Output : not null access Ada.Streams.Root_Stream_Type'Class;
								Tag : in String; Template : in Ase.Web.Producers.Template) is
							begin
								if Tag = "works" then
									Write(Output, "<select id=""work"" name=""work"">" &
										"<option value=""-1"" selected=""selected"">(既定)</option>");
									for Position in Cast.Works'Range loop
										declare
											Item : Villages.Casts.Work renames Cast.Works(Position);
										begin
											if Item.Name /= "" then
												Write(Output, "<option value=""");
												Write(Output, To_String(Position));
												Write(Output, """>");
												Write(Output, +Item.Name);
												case Item.Sex is
													when Villages.Male => Write(Output, " (男性職)");
													when Villages.Female => Write(Output, " (女性職)");
													when others => null;
												end case;
												if Item.Nominated then
													Write(Output, " (指名職)");
												end if;
												Write(Output, "</option>");
											end if;
										end;
									end loop;
									Write(Output, "</select>");
								elsif Tag = "names" then
									Write(Output, "<select id=""name"" name=""name"">");
									declare
										type Sex_To_String is array(Villages.Person_Sex) of String(1 .. 9);
										Sex_Name : constant Sex_To_String := (" (男性)", " (女性)");
									begin
										for Position in Cast.People'Range loop
											declare
												Item : Villages.Person_Type renames Cast.People(Position);
											begin
												if Item.Name /= "" then
													Write(Output, "<option value=""");
													Write(Output, To_String(Position));
													Write(Output, """>");
													Write(Output, +Item.Name);
													Write(Output, Sex_Name(Item.Sex));
													Write(Output, "</option>");
												end if;
											end;
										end loop;
									end;
									Write(Output, "</select>");
								elsif Tag = "request" then
									Write(Output, "<select id=""request"" name=""request"">");
									for I in Villages.Requested_Role loop
										Write(Output, "<option value=""");
										Write(Output, Villages.Requested_Role'Image(I));
										Write(Output, """>");
										Write(Output, Image(I));
										Write(Output, "</option>");
									end loop;
									Write(Output, "</select>");
								else
									Handle_Villages(Output, Tag, Template, Object,
										Village_Id, Village, Day, User_Id => User_Id, User_Password => User_Password);
								end if;
							end Handle_Entry;
						begin
							Villages.Casts.Load(Cast);
							Villages.Casts.Exclude_Taken(Cast, Village);
							Ase.Web.Producers.Produce(Output, Template, "entry", Handler => Handle_Entry'Access);
						end;
					end if;
				end if;
			elsif Tag = "rule" then
				if Day = 0 and then Tip_Showed then
					Rule.Rule_Panel(Object, Output,
						Template => Template'Address, -- avoiding compiler's bug of gcc 4.4.0
						Village_Id => Village_Id, Village => Village, Player => Player_Index >= 0,
						User_Id => User_Id, User_Password => User_Password);
				end if;
			elsif Tag = "next" then
				if Day < Village.Today and then Tip_Showed then
					declare
						procedure Handle_Next(Output : not null access Ada.Streams.Root_Stream_Type'Class;
							Tag : in String; Template : in Ase.Web.Producers.Template) is
						begin
							if Tag = "uri" then
								Link(Renderer'Class(Object), Output, Village_Id, Day + 1,
									User_Id => User_Id, User_Password => User_Password);
							else
								raise Program_Error;
							end if;
						end Handle_Next;
					begin
						Ase.Web.Producers.Produce(Output, Template, Handler => Handle_Next'Access);
					end;
				end if;
			elsif Tag = "scroll" then
				if Village.State /= Villages.Closed 
					and then Day = Village.Today 
					and then Player_Index >= 0
				then
					Ase.Web.Producers.Produce(Output, Template);
				end if;
			else
				Handle_Villages(Output, Tag, Template, Object,
					Village_Id, Village, Day, User_Id => User_Id, User_Password => User_Password);
			end if;
		end Handle;
		File: Ada.Streams.Stream_IO.File_Type;
	begin
		Ada.Streams.Stream_IO.Open(File, Ada.Streams.Stream_IO.In_File, 
			Object.Configuration.Template_Village_File_Name.all);
		begin
			Target_Day := Village.Today;
			if Village.Time = Villages.Night then
				Target_Day := Target_Day - 1;
			end if;
			Ase.Web.Producers.Produce(Output, 
				Ase.Web.Producers.Read(Ada.Streams.Stream_IO.Stream(File), Natural(Ada.Streams.Stream_IO.Size(File))), 
				Handler => Handle'Access);
		exception
			when others =>
				Ada.Streams.Stream_IO.Close(File);
				raise;
		end;
		Ada.Streams.Stream_IO.Close(File);
	end Village_Page;
	
	procedure Preview_Page(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Village_Id : Villages.Lists.Village_Id;
		Village : Villages.Village_Type; 
		Message : Villages.Message;
		User_Id : String;
		User_Password : String)
	is
		use type Ase.Web.HTML_Version;
		procedure Handle(Output : not null access Ada.Streams.Root_Stream_Type'Class;
			Tag : in String; Template : in Ase.Web.Producers.Template) is
		begin
			if Tag = "value" then
				Write(Output, '"');
				Write(Output, Ase.Web.Markup_Entity(+Message.Text,
					Line_Break => HTML_Version(Renderer'Class(Object)) = Ase.Web.XHTML));
				Write(Output, '"');
			elsif Tag = "longer" then
				if Ada.Strings.Unbounded.Length(Message.Text) > Max_Length_Of_Message then
					Ase.Web.Producers.Produce(Output, Template);
				end if;
			elsif Tag = "ok" then
				if Ada.Strings.Unbounded.Length(Message.Text) <= Max_Length_Of_Message then
					Ase.Web.Producers.Produce(Output, Template, Handler => Handle'Access);
				end if;
			else
				Handle_Messages(Output, Tag, Template, Object, 
					Village_Id, Village, Village.Today, Message, Message.Time, User_Id => User_Id, User_Password => User_Password);
			end if;
		end Handle;
		File: Ada.Streams.Stream_IO.File_Type;
	begin
		Ada.Streams.Stream_IO.Open(File, Ada.Streams.Stream_IO.In_File, 
			Object.Configuration.Template_Preview_File_Name.all);
		begin
			Ase.Web.Producers.Produce(Output, 
				Ase.Web.Producers.Read(Ada.Streams.Stream_IO.Stream(File), Natural(Ada.Streams.Stream_IO.Size(File))), 
				Handler => Handle'Access);
		exception
			when others =>
				Ada.Streams.Stream_IO.Close(File);
				raise;
		end;
		Ada.Streams.Stream_IO.Close(File);
	end Preview_Page;
	
	procedure Target_Page(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Village_Id : Villages.Lists.Village_Id;
		Village : Villages.Village_Type;
		Player : Natural;
		Target : Natural;
		User_Id : String;
		User_Password : String) 
	is
		Person : Villages.Person_Type renames Village.People(Player);
		Target_Person : Villages.Person_Type renames Village.People(Target);
		procedure Handle(Output : not null access Ada.Streams.Root_Stream_Type'Class;
			Tag : in String; Template : in Ase.Web.Producers.Template) is
		begin
			if Tag = "message" then
				case Person.Role is
					when Villages.Doctor =>
						Write(Output, Renderers.Name(Target_Person) & "を診察しますか？");
					when Villages.Detective =>
						if Target_Person.Records(Village.Today).Note = "" then
							Write(Output, "遺言を読み解くにはもう少しかかりそうですが、現時点で");
						end if;
						Write(Output, Renderers.Name(Target_Person) & "を調査しますか？");
					when others =>
						raise Program_Error;
				end case;
			elsif Tag = "button" then
				case Person.Role is
					when Villages.Doctor => Write(Output, "診察");
					when Villages.Detective => Write(Output, "調査");
					when others => raise Program_Error;
				end case;
			elsif Tag = "target" then
				Write(Output, '"');
				Write(Output, To_String(Target));
				Write(Output, '"');
			else
				Handle_Villages(Output, Tag, Template, Object,
					Village_Id, Village, Village.Today, User_Id => User_Id, User_Password => User_Password);
			end if;
		end Handle;
		File: Ada.Streams.Stream_IO.File_Type;
	begin
		Ada.Streams.Stream_IO.Open(File, Ada.Streams.Stream_IO.In_File, 
			Object.Configuration.Template_Target_File_Name.all);
		begin
			Ase.Web.Producers.Produce(Output, 
				Ase.Web.Producers.Read(Ada.Streams.Stream_IO.Stream(File), Natural(Ada.Streams.Stream_IO.Size(File))), 
				Handler => Handle'Access);
		exception
			when others =>
				Ada.Streams.Stream_IO.Close(File);
				raise;
		end;
		Ada.Streams.Stream_IO.Close(File);
	end Target_Page;
	
	procedure Produce(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		File_Name : in String;
		Handler : not null access procedure(Output : not null access Ada.Streams.Root_Stream_Type'Class; Tag : in String; Contents : Ase.Web.Producers.Template)) 
	is
		File: Ada.Streams.Stream_IO.File_Type;
	begin
		Ada.Streams.Stream_IO.Open(File, Ada.Streams.Stream_IO.In_File, File_Name);
		begin
			Ase.Web.Producers.Produce(Output,
				Ase.Web.Producers.Read(Ada.Streams.Stream_IO.Stream(File), Natural(Ada.Streams.Stream_IO.Size(File))), 
				Handler => Handler);
		exception
			when others =>
				Ada.Streams.Stream_IO.Close(File);
				raise;
		end;
		Ada.Streams.Stream_IO.Close(File);
	end Produce;
	
	procedure Link_Style_Sheet(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class) is
	begin
		Write(Output, '"');
		Write(Output, Object.Configuration.Style_Sheet_File_Name.all);
		Write(Output, '"');
	end Link_Style_Sheet;

	procedure User_Panel(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Template : in Ase.Web.Producers.Template;
		User_Id : in String;
		User_Password : in String;
		Link_To_User_Page : Boolean)
	is
		procedure Handle(Output : not null access Ada.Streams.Root_Stream_Type'Class;
			Tag : in String; Template : in Ase.Web.Producers.Template) is
		begin
			if Tag = "id" then
				if not Link_To_User_Page or else User_Id = Tabula.Users.Administrator then
					Write(Output, User_Id);
				else
					Write(Output, "<a href=");
					Link(Renderer'Class(Object), Output, 
						User_Id => User_Id, User_Password => User_Password, User_Page => True);
					Write(Output, '>');
					Write(Output, User_Id);
					Write(Output, "</a>");
				end if;
			elsif Tag = "administrator" then
				if User_Id = Tabula.Users.Administrator then
					Ase.Web.Producers.Produce(Output, Template, Handler => Handle'Access);
				end if;
			else
				Handle_Users(Output, Tag, Template, Object,
					User_Id => User_Id, User_Password => User_Password);
			end if;
		end Handle;
		Extract : constant array(Boolean) of access constant String := (
			new String'("logoff"), new String'("logon"));
	begin
		Ase.Web.Producers.Produce(Output, Template, Extract(User_Id /= "").all, Handler => Handle'Access);
	end User_Panel;
	
	procedure Link(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Village_Id : Villages.Lists.Village_Id := Villages.Lists.Invalid_Village_Id;
		Day : Integer := -1;
		First : Integer := -1;
		Last : Integer := -1;
		Latest : Integer := -1;
		Log : Boolean := False;
		User_Id : in String;
		User_Password : in String;
		User_Page : Boolean := False) is
	begin
		Write(Output, '"');
		if User_Page then
			Write(Output, "?user=");
			Write(Output, User_Id);
		elsif Log then
			Write(Output, "./");
			Write(Output, Object.Configuration.Log_Directory.all);
			Write(Output, '/');
			Write(Output, Village_Id);
			Write(Output, "-0.html");
		else
			Write(Output, '?');
			if Village_Id /= Villages.Lists.Invalid_Village_Id then
				Write(Output, "village=");
				Write(Output, Village_Id);
				if Day >= 0 then
					Write(Output, "&day=");
					Write(Output, To_String(Day));
				end if;
			end if;
		end if;
		Write(Output, '"');
	end Link;
	
	procedure Link_Image(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		File_Name : in String) is
	begin
		Write(Output, '"');
		Write(Output, Object.Configuration.Image_Directory.all);
		Write(Output, "/");
		Write(Output, File_Name);
		Write(Output, '"');
	end Link_Image;
	
	procedure Day_Name(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Day : Natural;
		Today : Natural;
		State : Villages.Village_State)
	is
		use type Villages.Village_State;
	begin
		if Day = 0 then
			Write(Output, "プロローグ");
		elsif (State >= Villages.Epilogue) and (Today = Day) then
			Write(Output, "エピローグ");
		else
			Write(Output, Natural'Image(Day) & "日目");
		end if;
	end Day_Name;
	
	function HTML_Version(Object : in Renderer) return Ase.Web.HTML_Version is
	begin
		return Ase.Web.XHTML;
	end HTML_Version;
	
end Tabula.Renderers;
