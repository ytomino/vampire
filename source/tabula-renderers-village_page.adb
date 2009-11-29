-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Calendar.Formatting;
with Ada.Containers;
with Ada.Numerics.MT19937;
with Ada.Strings.Unbounded;
with Tabula.Calendar;
with Tabula.Casts.Load;
with Tabula.Users;
with Tabula.Vampires.Villages.Teaming;
procedure Tabula.Renderers.Village_Page (
	Object : in Renderer'Class;
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Village_Id : in Villages.Village_Id; 
	Village : not null access constant Vampires.Villages.Village_Type; 
	Day : in Natural;
	First, Last : in Integer := -1;
	User_Id : in String;
	User_Password : in String)
is
	use type Ada.Calendar.Time;
	use type Ada.Containers.Count_Type;
	use type Ada.Strings.Unbounded.Unbounded_String;
	use type Web.HTML_Version;
	use type Tabula.Vampires.Villages.Daytime_Preview_Mode;
	use type Tabula.Vampires.Villages.Execution_Mode;
	use type Tabula.Vampires.Villages.Message_Kind;
	use type Tabula.Vampires.Villages.Monster_Side_Mode;
	use type Tabula.Vampires.Villages.Person_Role;
	use type Tabula.Vampires.Villages.Person_State;
	use type Tabula.Villages.Village_State;
	use type Tabula.Villages.Village_Time;
	function "+" (S : Ada.Strings.Unbounded.Unbounded_String) return String renames Ada.Strings.Unbounded.To_String;
	function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String renames Ada.Strings.Unbounded.To_Unbounded_String;
	
	Line_Break : constant Character := Ascii.LF;
	
	Role_Image_File_Name : constant array(Vampires.Villages.Person_Role) of not null access constant String := (
		Vampires.Villages.Gremlin => new String'("gremlin.png"),
		Vampires.Villages.Vampire_Role => new String'("vampire.png"),
		Vampires.Villages.Servant => new String'("servant.png"),
		Vampires.Villages.Inhabitant | Vampires.Villages.Loved_Inhabitant | Vampires.Villages.Unfortunate_Inhabitant => new String'("inhabitant.png"),
		Vampires.Villages.Detective => new String'("detective.png"),
		Vampires.Villages.Doctor => new String'("doctor.png"),
		Vampires.Villages.Astronomer => new String'("astronomer.png"),
		Vampires.Villages.Hunter => new String'("hunter.png"),
		Vampires.Villages.Lover | Vampires.Villages.Sweetheart_M | Vampires.Villages.Sweetheart_F => new String'("sweetheart.png"));
	
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
				"吸血鬼は実在し……この村に紛れているのです！ ")),
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
	
	For_Execution_Message : constant String := "誰かが古びた杭を持って来ました……これしかないのでしょうか……。 ";
	
	function Stage(Village : in Vampires.Villages.Village_Type) return Stage_Kind is
		L : constant Natural := Ada.Strings.Unbounded.Length(Village.Name);
	begin
		if L >= 3 and then Ada.Strings.Unbounded.Slice(Village.Name, L - 2, L) = "城" then
			return A_Castle;
		else
			return A_Village;
		end if;
	end Stage;
	
	function Image(Role : Vampires.Villages.Requested_Role) return String is
	begin
		case Role is
			when Vampires.Villages.Inhabitant => return "村人";
			when Vampires.Villages.Vampire => return "吸血鬼";
			when Vampires.Villages.Servant => return "使徒";
			when Vampires.Villages.Detective => return "探偵";
			when Vampires.Villages.Astronomer => return "天文家";
			when Vampires.Villages.Doctor => return "医者";
			when Vampires.Villages.Hunter => return "猟師";
			when Vampires.Villages.Sweetheart => return "恋人";
			when Vampires.Villages.Random => return "ランダム";
			when Vampires.Villages.Rest => return "希望無し";
			when Vampires.Villages.Village_Side => return "村側";
			when Vampires.Villages.Vampire_Side => return "吸血鬼側";
			when Vampires.Villages.Gremlin => return "妖魔";
		end case;
	end Image;
	
	function Image(Role : Vampires.Villages.Person_Role) return String is
	begin
		case Role is
			when Vampires.Villages.Inhabitant | Vampires.Villages.Loved_Inhabitant => return "善良な村人";
			when Vampires.Villages.Unfortunate_Inhabitant => return "数奇な運命の村人";
			when Vampires.Villages.Vampire_K | Vampires.Villages.Vampire_Q | Vampires.Villages.Vampire_J => return "吸血鬼";
			when Vampires.Villages.Servant => return "吸血鬼の使徒";
			when Vampires.Villages.Detective => return "探偵";
			when Vampires.Villages.Astronomer => return "天文家";
			when Vampires.Villages.Doctor => return "医者";
			when Vampires.Villages.Hunter => return "猟師";
			when Vampires.Villages.Lover | Vampires.Villages.Sweetheart_M | Vampires.Villages.Sweetheart_F => return "恋する村人";
			when Vampires.Villages.Gremlin => return "妖魔";
		end case;
	end Image;
	
	function Short_Image (Role : Vampires.Villages.Person_Role) return String is
	begin
		case Role is
			when Vampires.Villages.Inhabitant | Vampires.Villages.Loved_Inhabitant => return "村";
			when Vampires.Villages.Unfortunate_Inhabitant => return "奇";
			when Vampires.Villages.Vampire_K | Vampires.Villages.Vampire_Q | Vampires.Villages.Vampire_J => return "鬼";
			when Vampires.Villages.Servant => return "使";
			when Vampires.Villages.Detective => return "探";
			when Vampires.Villages.Astronomer => return "天";
			when Vampires.Villages.Doctor => return "医";
			when Vampires.Villages.Hunter => return "猟";
			when Vampires.Villages.Lover | Vampires.Villages.Sweetheart_M | Vampires.Villages.Sweetheart_F => return "恋";
			when Vampires.Villages.Gremlin => return "妖";
		end case;
	end Short_Image;
	
	function Fatalities_List(Village : Vampires.Villages.Village_Type; Day : Natural; Executed : Integer) return String is
		Result : Ada.Strings.Unbounded.Unbounded_String;
	begin
		pragma Assert(Day >= 2);
		for I in Village.People.First_Index .. Village.People.Last_Index loop
			declare
				P : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(I).Element.all;
			begin
				if P.Records.Constant_Reference(Day - 1).Element.State /= Vampires.Villages.Died
					and then P.Records.Constant_Reference(Day).Element.State = Vampires.Villages.Died
					and then Executed /= I
				then
					if Result /= Ada.Strings.Unbounded.Null_Unbounded_String then
						Ada.Strings.Unbounded.Append(Result, "、");
					else
						Result := +"翌朝、";
					end if;
					Ada.Strings.Unbounded.Append(Result, Name(P));
				end if;
			end;
		end loop;
		if Result /= Ada.Strings.Unbounded.Null_Unbounded_String then
			Ada.Strings.Unbounded.Append(Result, "の遺体が見つかりました……！");
		end if;
		return +Result;
	end Fatalities_List;
	
	function Survivors_List(Village : Vampires.Villages.Village_Type; Day : Natural) return String is
		Result : Ada.Strings.Unbounded.Unbounded_String;
	begin
		for I in Village.People.First_Index .. Village.People.Last_Index loop
			declare
				P : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(I).Element.all;
			begin
				if P.Records.Constant_Reference(Day).Element.State /= Vampires.Villages.Died then
					if Result /= Ada.Strings.Unbounded.Null_Unbounded_String then
						Ada.Strings.Unbounded.Append(Result, "、");
					end if;
					Ada.Strings.Unbounded.Append(Result, Name(P));
				end if;
			end;
		end loop;
		Ada.Strings.Unbounded.Append(Result, "が生存者です。 ");
		return +Result;
	end Survivors_List;
	
	function Vote_Report (
		Village : Vampires.Villages.Village_Type;
		Day : Natural;
		Provisional : Boolean;
		Player_Index : Integer) return String
	is
		Result : Ada.Strings.Unbounded.Unbounded_String;
	begin
		for I in Village.People.First_Index .. Village.People.Last_Index loop
			declare
				P : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(I).Element.all;
				V : Integer;
			begin
				if Provisional then
					V := P.Records.Constant_Reference(Day).Element.Provisional_Vote;
				else
					V := P.Records.Constant_Reference(Day).Element.Vote;
				end if;
				if V in Village.People.First_Index .. Village.People.Last_Index then
					if Village.State >= Villages.Epilogue or else
						Player_Index = I or else
						Village.Execution = Vampires.Villages.Provisional_Voting_From_Second
					then
						declare
							T : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(V).Element.all;
						begin
							Ada.Strings.Unbounded.Append(Result, Name(P) & "は" & Name(T) & "に投票しました。" & Line_Break);
						end;
					end if;
				end if;
			end;
		end loop;
		return +Result;
	end Vote_Report;
	
	function Vote_Count (
		Village : Vampires.Villages.Village_Type;
		Day : Natural;
		Provisional : Boolean;
		Executed: Integer) return String
	is
		Result : Ada.Strings.Unbounded.Unbounded_String;
		Voted : array(Village.People.First_Index .. Village.People.Last_Index) of Natural := (others => 0);
		Max_Voted : Natural := 0;
	begin
		for I in Voted'Range loop
			declare
				P : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(I).Element.all;
				V : Integer;
			begin
				if Provisional then
					V := P.Records.Constant_Reference(Day).Element.Provisional_Vote;
				else
					V := P.Records.Constant_Reference(Day).Element.Vote;
				end if;
				if V in Village.People.First_Index .. Village.People.Last_Index then
					Voted(V) := Voted(V) + 1;
					if Voted(V) > Max_Voted then
						Max_Voted := Voted(V);
					end if;
				end if;
			end;
		end loop;
		for Count in reverse 1 .. Max_Voted loop
			for I in Voted'Range loop
				if Voted(I) = Count then
					declare
						P : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(I).Element.all;
					begin
						Ada.Strings.Unbounded.Append(Result, To_String(Voted(I)) & "票が" & Name(P) & "に集まりました。" & Line_Break);
					end;
				end if;
			end loop;
		end loop;
		if Provisional then
			declare
				First : Boolean := True;
			begin
				Ada.Strings.Unbounded.Append (Result, "仮投票の結果、");
				for I in Village.People.First_Index .. Village.People.Last_Index loop
					declare
						The_Person : Vampires.Villages.Person_Type renames Village.People.Constant_Reference (I).Element.all;
					begin
						if The_Person.Records.Constant_Reference (Day).Element.Candidate and then
							The_Person.Records.Constant_Reference (Day).Element.State /= Vampires.Villages.Died
						then
							if not First then
								Ada.Strings.Unbounded.Append (Result, "、");
							end if;
							Ada.Strings.Unbounded.Append (Result, Name (The_Person));
							First := False;
						end if;
					end;
				end loop;
				Ada.Strings.Unbounded.Append(Result, "が本投票の候補になります。");
			end;
		else
			Ada.Strings.Unbounded.Append(Result, Name(Village.People.Constant_Reference(Executed).Element.all) & "は心臓に杭を打ち込まれました。");
		end if;
		return +Result;
	end Vote_Count;
	
	function Vampires_List(Village : Vampires.Villages.Village_Type) return String is
		Result : Ada.Strings.Unbounded.Unbounded_String := +"その夜、草木も眠る頃、人知れず月を舞う影がありました……。 ";
	begin
		for I in Vampires.Villages.Vampire_Role loop
			for Position in Village.People.First_Index .. Village.People.Last_Index loop
				declare
					P : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(Position).Element.all;
				begin
					if P.Role = I then
						if I /= Vampires.Villages.Vampire_K then
							Ada.Strings.Unbounded.Append(Result, "、");
						end if;
						Ada.Strings.Unbounded.Append(Result, Name(P));
					end if;
				end;
			end loop;
		end loop;
		Ada.Strings.Unbounded.Append(Result, "。 村を見下ろすと、誰かが夜道を早足で歩いています……。 ");
		return +Result;
	end Vampires_List;
	
	function Breakdown_List(Village : Vampires.Villages.Village_Type) return String is
		Detective, Astronomer, Doctor, Hunter, Sweetheart, Lover, Unfortunate, Servant, Gremlin : Boolean := False;
		Vampire_Count : Natural := 0;
		Village_Side_Capabilityperson : Natural := 0;
		procedure Countup(Role : Vampires.Villages.Person_Role) is
		begin
			case Role is
				when Vampires.Villages.Detective =>
					Detective := True;
					Village_Side_Capabilityperson := Village_Side_Capabilityperson + 1;
				when Vampires.Villages.Astronomer =>
					Astronomer := True;
					Village_Side_Capabilityperson := Village_Side_Capabilityperson + 1;
				when Vampires.Villages.Doctor =>
					Doctor := True;
					Village_Side_Capabilityperson := Village_Side_Capabilityperson + 1;
				when Vampires.Villages.Hunter =>
					Hunter := True;
					Village_Side_Capabilityperson := Village_Side_Capabilityperson + 1;
				when Vampires.Villages.Lover =>
					Lover := True;
					Village_Side_Capabilityperson := Village_Side_Capabilityperson + 1;
				when Vampires.Villages.Sweetheart_M | Vampires.Villages.Sweetheart_F =>
					Sweetheart := True;
					Village_Side_Capabilityperson := Village_Side_Capabilityperson + 1;
				when Vampires.Villages.Unfortunate_Inhabitant =>
					Unfortunate := True;
					Village_Side_Capabilityperson := Village_Side_Capabilityperson + 1;
				when Vampires.Villages.Servant =>
					Servant := True;
				when Vampires.Villages.Vampire_Role =>
					Vampire_Count := Vampire_Count + 1;
				when Vampires.Villages.Gremlin =>
					Gremlin := True;
				when others =>
					null;
			end case;
		end Countup;
		Result : Ada.Strings.Unbounded.Unbounded_String;
	begin
		if Village.Execution = Vampires.Villages.Dummy_Killed_And_From_First then
			Result := "地主さんを含む" & (+To_String(1 + Integer(Village.People.Length)));
			Countup (Village.Dummy_Role);
		else
			Result := +To_String(Integer(Village.People.Length));
		end if;
		Ada.Strings.Unbounded.Append(Result, "人の村人の中には");
		for Position in Village.People.First_Index .. Village.People.Last_Index loop
			Countup(Village.People.Constant_Reference(Position).Element.Role);
		end loop;
		if Village.Teaming in Vampires.Villages.Hidings then
			Ada.Strings.Unbounded.Append(Result, To_String(Village_Side_Capabilityperson));
			Ada.Strings.Unbounded.Append(Result, "人の能力者");
		else
			if Detective then
				Ada.Strings.Unbounded.Append(Result, "、探偵");
			end if;
			if Astronomer then
				Ada.Strings.Unbounded.Append(Result, "、天文家");
			end if;
			if Doctor then
				Ada.Strings.Unbounded.Append(Result, "、医者");
			end if;
			if Hunter then
				Ada.Strings.Unbounded.Append(Result, "、猟師");
			end if;
			if Lover then
				Ada.Strings.Unbounded.Append(Result, "、片想い");
			end if;
			if Sweetheart then
				Ada.Strings.Unbounded.Append(Result, "、恋人");
			end if;
			if Unfortunate then
				Ada.Strings.Unbounded.Append(Result, "、数奇な運命の村人");
			end if;
		end if;
		Ada.Strings.Unbounded.Append(Result, "がいます。 ");
		if Village.Monster_Side = Vampires.Villages.Shuffling then
			Ada.Strings.Unbounded.Append(Result, "吸血鬼の全貌はわかりません……。 ");
		else
			Ada.Strings.Unbounded.Append(Result, "そして昨夜、月明かりに照らし出された人影が");
			Ada.Strings.Unbounded.Append(Result, To_String(Vampire_Count));
			Ada.Strings.Unbounded.Append(Result, "つ……。 ");
			if Servant then
				case Village.Servant_Knowing is
					when Vampires.Villages.None =>
						Ada.Strings.Unbounded.Append(Result, "それを崇める者……。 ");
					when Vampires.Villages.Vampire_K =>
						Ada.Strings.Unbounded.Append(Result, "吸血鬼の王を目撃し魅了された者……。 ");
					when Vampires.Villages.All_Vampires =>
						Ada.Strings.Unbounded.Append(Result, "吸血鬼の集いを目撃し魅了された者……。 ");
				end case;
			end if;
			if Gremlin then
				Ada.Strings.Unbounded.Append(Result, "さらに忍び寄る魔の手……。 ");
			end if;
		end if;
		return +Result;
	end Breakdown_List;
	
	function Doctor_Cure_Message(Village : Vampires.Villages.Village_Type; Message : Vampires.Villages.Message) return String is
		Subject : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(Message.Subject).Element.all;
		Target : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(Message.Target).Element.all;
		Showing_Result : constant array(Vampires.Villages.Doctor_Message_Kind) of not null access constant String := (
			Vampires.Villages.Doctor_Found_Infection | Vampires.Villages.Doctor_Found_Infection_Preview |
			Vampires.Villages.Doctor_Cure | Vampires.Villages.Doctor_Cure_Preview =>
				new String'("を診察し、首筋に牙の跡を見つけました。" & Line_Break & "薬が効くことを祈りましょう。"),
			Vampires.Villages.Doctor_Failed | Vampires.Villages.Doctor_Failed_Preview =>
				new String'("を診察しましたが、異常は見当たりませんでした。"),
			Vampires.Villages.Doctor_Found_Gremlin | Vampires.Villages.Doctor_Found_Gremlin_Preview =>
				new String'("を診察しました。" & Line_Break & "……妖魔だ！"));
	begin
		return Name(Subject) & "は" & Name(Target) & Showing_Result(Message.Kind).all;
	end Doctor_Cure_Message;
	
	function Detective_Survey_Message(Village : Vampires.Villages.Village_Type; Message : Vampires.Villages.Message) return String is
		Subject : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(Message.Subject).Element.all;
		function Showing_Role(Role : Vampires.Villages.Person_Role) return String is
		begin
			case Role is
				when Vampires.Villages.Gremlin | Vampires.Villages.Vampire_K .. Vampires.Villages.Vampire_J => return "人間では無かった";
				when others => return Image(Role) & "だった";
			end case;
		end Showing_Role;
		Role : Vampires.Villages.Person_Role;
	begin
		case Vampires.Villages.Detective_Message_Kind (Message.Kind) is
			when Vampires.Villages.Detective_Survey | Vampires.Villages.Detective_Survey_Preview =>
				declare
					Target : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(Message.Target).Element.all;
				begin
					if Village.Daytime_Preview = Vampires.Villages.Message_Only and then Message.Kind = Vampires.Villages.Detective_Survey_Preview then
						return Name(Subject) & "は" & Name(Target) & "を調査しました。";
					else
						Role := Target.Role;
						return Name(Subject) & "は" & Name(Target) & "を調査しました。" & Line_Break &
							"どうやら" & Showing_Role(Role) & "ようです。" & Line_Break;
					end if;
				end;
			when Vampires.Villages.Detective_Survey_Victim =>
				return Name(Subject) & "は地主さんを調査しました。" & Line_Break &
					"どうやら" & Showing_Role (Village.Dummy_Role) & "ようです。" & Line_Break;
		end case;
	end Detective_Survey_Message;
	
	function Astronomer_Observation_Message(Village : Vampires.Villages.Village_Type; Message : Vampires.Villages.Message) return String is
		Subject : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(Message.Subject).Element.all;
		Target : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(Message.Target).Element.all;
		function Showing_Result return String is
		begin
			if Target.Role in Vampires.Villages.Vampire_Role
				or else Target.Role = Vampires.Villages.Gremlin
				or else Target.Records.Constant_Reference(Message.Day - 1).Element.State = Vampires.Villages.Infected
			then
				return "観測していて、人影が飛び立つのを目撃してしまいました。";
			else
				return "観測していました。";
			end if;
		end Showing_Result;
	begin
		return Name(Subject) & "は" & Name(Target) & "の家の上空を" & Showing_Result;
	end Astronomer_Observation_Message;
	
	function Hunter_Guard_Message(Village : Vampires.Villages.Village_Type; Message : Vampires.Villages.Message) return String is
		Subject : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(Message.Subject).Element.all;
	begin
		case Message.Kind is
			when Vampires.Villages.Hunter_Nothing_With_Silver =>
				return Name(Subject) & "は銃に銀の弾丸を込めていましたが、その夜は何事もありませんでした。";
			when Vampires.Villages.Hunter_Infected_With_Silver =>
				return Name(Subject) & "は銀の弾丸で吸血鬼を撃ち抜きました。";
			when Vampires.Villages.Hunter_Killed_With_Silver =>
				return Name(Subject) & "は自らの命と引き換えに、銀の弾丸で吸血鬼を撃ち抜きました。";
			when others =>
				declare
					Target : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(Message.Target).Element.all;
				begin
					case Vampires.Villages.Hunter_Message_Kind (Message.Kind) is
						when Vampires.Villages.Hunter_Guard =>
							return Name(Subject) & "は" & Name(Target) & "を吸血鬼から守り抜きました。";
						when Vampires.Villages.Hunter_Guard_With_Silver =>
							return Name(Subject) & "は" & Name(Target) & "を守り、銀の弾丸で吸血鬼を撃ち抜きました。";
						when Vampires.Villages.Hunter_Nothing_With_Silver | Vampires.Villages.Hunter_Infected_With_Silver | Vampires.Villages.Hunter_Killed_With_Silver =>
							raise Program_Error;
						when Vampires.Villages.Hunter_Failed =>
							return Name(Subject) & "は" & Name(Target) & "を守っていました。";
						when Vampires.Villages.Hunter_Failed_With_Silver =>
							return Name(Subject) & "は" & Name(Target) & "を銀の弾丸で守っていました。";
					end case;
				end;
		end case;
	end Hunter_Guard_Message;
	
	function Servant_Knew_Message(Village : Vampires.Villages.Village_Type; Message : Vampires.Villages.Message) return String is
		Subject : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(Message.Subject).Element.all;
		Result : Ada.Strings.Unbounded.Unbounded_String := +(Name(Subject) & "は見てしまいました。");
	begin
		case Vampires.Villages.Servant_Message_Kind (Message.Kind) is
			when Vampires.Villages.Servant_Knew_Vampire_K =>
				Ada.Strings.Unbounded.Append (Result, "吸血鬼の王は");
				for Position in Village.People.First_Index .. Village.People.Last_Index loop
					declare
						P : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(Position).Element.all;
					begin
						if P.Role = Vampires.Villages.Vampire_K then
							Ada.Strings.Unbounded.Append (Result, Name(P));
						end if;
					end;
				end loop;
				Ada.Strings.Unbounded.Append (Result, "です。");
			when Vampires.Villages.Servant_Knew_Vampires =>
				Ada.Strings.Unbounded.Append (Result, "吸血鬼は");
				for Role in Vampires.Villages.Vampire_Role loop
					for Position in Village.People.First_Index .. Village.People.Last_Index loop
						declare
							P : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(Position).Element.all;
						begin
							if P.Role = Role then
								Ada.Strings.Unbounded.Append (Result, Name(P));
							end if;
						end;
					end loop;
				end loop;
				Ada.Strings.Unbounded.Append (Result, "です。");
		end case;
		return +Result;
	end Servant_Knew_Message;
	
	function Vampire_Murder_Message(Village : Vampires.Villages.Village_Type; Message : Vampires.Villages.Message;
		Executed : Integer) return String
	is
		Result : Ada.Strings.Unbounded.Unbounded_String;
		Subject : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(Message.Subject).Element.all;
		Target : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(Message.Target).Element.all;
	begin
		if Subject.Role in Vampires.Villages.Vampire_Role then
			for Role in Vampires.Villages.Vampire_Role loop
				for I in Village.People.First_Index .. Village.People.Last_Index loop
					if I /= Executed then
						declare
							P : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(I).Element.all;
						begin
							if P.Role = Role then
								declare
									V : constant Integer := P.Records.Constant_Reference(Message.Day - 1).Element.Target;
								begin
									if V >= 0 then
										declare
											T : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(V).Element.all;
										begin
											Ada.Strings.Unbounded.Append(Result, Name(P) & "は" & Name(T) & "に目をつけました。" & Line_Break);
										end;
									end if;
								end;
							end if;
						end;
					end if;
				end loop;
			end loop;
		else
			Ada.Strings.Unbounded.Append(Result, Name(Subject) & "は" & Name(Target) & "に目をつけました。" & Line_Break);
		end if;
		Ada.Strings.Unbounded.Append(Result, "吸血鬼は" & Name(Target) & "を");
		case Vampires.Villages.Vampire_Message_Kind(Message.Kind) is
			when Vampires.Villages.Vampire_Murder =>
				Ada.Strings.Unbounded.Append(Result, "襲いました。");
			when Vampires.Villages.Vampire_Murder_And_Killed =>
				Ada.Strings.Unbounded.Append(Result, "襲い、抵抗を受け殺されました。");
			when Vampires.Villages.Vampire_Infection =>
				Ada.Strings.Unbounded.Append(Result, "感染させました。");
			when Vampires.Villages.Vampire_Infection_And_Killed =>
				Ada.Strings.Unbounded.Append(Result, "感染させ、抵抗を受け殺されました。");
			when Vampires.Villages.Vampire_Failed =>
				Ada.Strings.Unbounded.Append(Result, "襲おうとしましたが、何者かに妨げられました。");
			when Vampires.Villages.Vampire_Failed_And_Killed =>
				Ada.Strings.Unbounded.Append(Result, "襲おうとしましたが、何者かに妨げられ殺されました。");
		end case;
		return +Result;
	end Vampire_Murder_Message;
	
	procedure Rule_Panel (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Template : in Web.Producers.Template;
		Village_Id : in Villages.Village_Id;
		Village : not null access constant Vampires.Villages.Village_Type; 
		Player : in Boolean;
		User_Id : in String;
		User_Password : in String)
	is
		Changable : constant Boolean := Player and then Village.Today = 0 and then Village.No_Commit;
		procedure Handle (
			Output : not null access Ada.Streams.Root_Stream_Type'Class;
			Tag : in String;
			Template : in Web.Producers.Template) is
		begin
			if Tag = "items" then
				if Changable then
					declare
						procedure Process (Item : in Villages.Root_Option_Item'Class) is
						begin
							if Item.Available then
								Write (Output, "<select name=""");
								Write (Output, Item.Name);
								Write (Output, """>");
								declare
									procedure Process (
										Value : in String;
										Selected : in Boolean;
										Message : in String;
										Unrecommended : in Boolean) is
									begin
										Write (Output, "<option value=""");
										Write (Output, Value);
										Write (Output, '"');
										if Selected then
											Write (Output, " selected=""selected""");
										end if;
										Write (Output, '>');
										Web.Write_In_HTML (Output, Object.HTML_Version, Message);
										if Unrecommended then
											Write(Output, " お薦めしません。");
										end if;
										if Selected then
											Write(Output, " *");
										end if;
										Write (Output, "</option>");
									end Process;
								begin
									Tabula.Villages.Iterate (Item, Process'Access);
								end;
								Write(Output, "</select>");
								Web.Producers.Produce (Output, Template);
							end if;
						end Process;
					begin
						Vampires.Villages.Iterate (Village, Process'Access);
					end;
				else
					declare
						procedure Process (Item : in Villages.Root_Option_Item'Class) is
						begin
							if Item.Available and then (Player or else Item.Changed) then
								declare
									procedure Process (
										Value : in String;
										Selected : in Boolean;
										Message : in String;
										Unrecommended : in Boolean) is
									begin
										if Selected then
											if Item.Changed and then Village.State /= Villages.Closed then
												Write (Output, "<em>");
											end if;
											Write (Output, Message);
											if Village.State /= Villages.Closed and then Unrecommended then
												Write (Output, " お薦めしません。");
											end if;
											if Item.Changed and then Village.State /= Villages.Closed then
												Write (Output, "</em>");
											end if;
											Write (Output, ' ' & Ascii.LF);
										end if;
									end Process;
								begin
									Tabula.Villages.Iterate (Item, Process'Access);
								end;
							end if;
						end Process;
					begin
						Vampires.Villages.Iterate (Village, Process'Access);
					end;
				end if;
			elsif Tag = "uri" then
				Link(Object, Output, Village_Id => Village_Id,
					User_Id => User_Id, User_Password => User_Password);
			else
				raise Program_Error with "Invalid template """ & Tag & """";
			end if;
		end Handle;
		Extract : constant array(Boolean) of access constant String := (
			new String'("static"), new String'("changable"));
	begin
		if Player 
			or else Village.Day_Duration < 24 * 60 * 60.0
			or else Village.Option_Changed
		then
			Web.Producers.Produce (Output, Template, Extract (Changable).all, Handler => Handle'Access);
		end if;
		if Village.State <= Villages.Opened and then Village.People.Length >= 3 then
			declare
				Sets : constant Vampires.Villages.Teaming.Role_Set_Array :=
					Vampires.Villages.Teaming.Possibilities (
						People_Count => Village.People.Length,
						Male_And_Female => Vampires.Villages.Male_And_Female (Village.People),
						Execution => Village.Execution,
						Teaming => Village.Teaming,
						Unfortunate => Village.Unfortunate,
						Monster_Side => Village.Monster_Side);
			begin
				Write (Output, "<ul>");
				for I in Sets'Range loop
					Write (Output, "<li>");
					for J in Vampires.Villages.Person_Role loop
						for K in 1 .. Sets (I)(J) loop
							Write (Output, Short_Image (J));
						end loop;
					end loop;
					Write (Output, "</li>");
				end loop;
				Write (Output, "</ul>");
			end;
		end if;
	end Rule_Panel;
	
	Target_Day : Natural;
	
	procedure Vote_Form (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Player : Integer;
		Kind : Vampires.Villages.Person_Role;
		Special: Boolean;
		Current : Integer;
		Current_Special : Boolean;
		Message : String;
		Button : String)
	is
		Including : Boolean;
	begin
		if Object.HTML_Version = Web.XHTML then
			Write(Output, "<form method=""POST"" class=""inner"">" & Line_Break &
				"<table><tr>" & Line_Break & "<td class=""input"">");
		else
			Write(Output, "<form method=""POST"" action=");
			Link (Object, Output, Village_Id => Village_Id,
				User_Id => User_Id, User_Password => User_Password);
			Write(Output, ">");
		end if;
		Write(Output, Message);
		Write(Output, " <select name=""target"">" & Line_Break);
		for Position in Village.People.First_Index .. Village.People.Last_Index loop
			if Position /= Player then
				case Kind is
					when Vampires.Villages.Inhabitant =>
						Including := Village.People.Constant_Reference(Position).Element.Records.Constant_Reference(Target_Day).Element.State /= Vampires.Villages.Died
							and then Village.People.Constant_Reference(Position).Element.Records.Constant_Reference(Target_Day).Element.Candidate;
					when Vampires.Villages.Detective =>
						Including := Village.People.Constant_Reference(Position).Element.Records.Constant_Reference(Target_Day).Element.State = Vampires.Villages.Died;
					when Vampires.Villages.Vampire_Role =>
						Including := Village.People.Constant_Reference(Position).Element.Records.Constant_Reference(Target_Day).Element.State /= Vampires.Villages.Died
							and then Village.People.Constant_Reference(Position).Element.Role not in Vampires.Villages.Vampire_Role;
					when others =>
						Including := Village.People.Constant_Reference(Position).Element.Records.Constant_Reference(Target_Day).Element.State /= Vampires.Villages.Died;
				end case;
				if Including then
					Write(Output, "<option value=""" & To_String(Position) & """");
					if Current = Position then
						Write(Output, " selected=""selected""");
					end if;
					Write(Output, ">");
					Write(Output, Renderers.Name(Village.People.Constant_Reference(Position).Element.all));
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
		case Kind is
			when Vampires.Villages.Hunter =>
				if Special then
					Write(Output, " <input name=""special"" type=""checkbox"" ");
					if Current_Special then
						Write(Output, "checked=""checked"" ");
					end if;
					Write(Output, "/>銀の弾丸");
				end if;
			when others =>
				null;
		end case;
		if Object.HTML_Version = Web.XHTML then
			Write(Output, "</td>" & Line_Break & "<td class=""button"">");
		end if;
		Write(Output, "<input type=""submit"" value=""" & Button & """ />");
		if Object.HTML_Version = Web.XHTML then
			Write(Output, "</td>" & Line_Break & "</tr></table>" & Line_Break);
		end if;
		Write(Output, "<input type=""hidden"" name=""cmd"" value=""");
		case Kind is
			when Vampires.Villages.Inhabitant =>
				Write(Output, "vote");
			when others =>
				Write(Output, "target");
		end case;
		Write(Output, """ />" & Line_Break & "</form>" & Line_Break);
	end Vote_Form;
	
	function Role_Text(Person : Vampires.Villages.Person_Type) return String is
		Setting : Vampires.Villages.Person_Record renames Person.Records.Constant_Reference(Village.Today).Element.all;
	begin
		if Setting.State = Vampires.Villages.Died then
			return "あなたは幽霊です。";
		else
			case Person.Role is
				when Vampires.Villages.Inhabitant | Vampires.Villages.Loved_Inhabitant | Vampires.Villages.Unfortunate_Inhabitant =>
					return "あなたは村人です。";
				when Vampires.Villages.Doctor =>
					if Setting.Target >= 0 then
						return "あなたは医者、" & Name(Village.People.Constant_Reference(Setting.Target).Element.all) & "を診察しました。";
					else
						return "あなたは医者です。";
					end if;
				when Vampires.Villages.Detective =>
					if Setting.Target >= 0 then
						return "あなたは探偵、" & Name(Village.People.Constant_Reference(Setting.Target).Element.all) & "を調査中です。";
					else
						return "あなたは探偵です。";
					end if;
				when Vampires.Villages.Astronomer =>
					if Person.Commited and Setting.Target >= 0 then
						return "あなたは天文家、" & Name(Village.People.Constant_Reference(Setting.Target).Element.all) & "の家の空を観測します。";
					else
						return "あなたは天文家です。";
					end if;
				when Vampires.Villages.Hunter =>
					if Person.Commited and Setting.Target >= 0 and Setting.Special then
						return "あなたは猟師、" & Name(Village.People.Constant_Reference(Setting.Target).Element.all) & "を銀の弾丸で守ります。";
					elsif Person.Commited and Setting.Target >= 0 then
						return "あなたは猟師、" & Name(Village.People.Constant_Reference(Setting.Target).Element.all) & "を守ります。";
					elsif Person.Commited and Setting.Special then
						return "あなたは猟師、銃には銀の弾丸を装填しています。";
					else
						return "あなたは猟師です。";
					end if;
				when Vampires.Villages.Lover =>
					for Position in Village.People.First_Index .. Village.People.Last_Index loop
						if Village.People.Constant_Reference(Position).Element.Role = Vampires.Villages.Loved_Inhabitant then
							return "あなたは" & Name(Village.People.Constant_Reference(Position).Element.all) & "に片想いです。";
						end if;
					end loop;
					pragma Assert(False);
					return "";
				when Vampires.Villages.Sweetheart_M | Vampires.Villages.Sweetheart_F =>
					for Position in Village.People.First_Index .. Village.People.Last_Index loop
						if Village.People.Constant_Reference(Position).Element.Role /= Person.Role
							and then Village.People.Constant_Reference(Position).Element.Role in Vampires.Villages.Sweetheart_M .. Vampires.Villages.Sweetheart_F
						then
							return "あなたは" & Name(Village.People.Constant_Reference(Position).Element.all) & "の恋人です。";
						end if;
					end loop;
					pragma Assert(False);
					return "";
				when Vampires.Villages.Servant =>
					return "あなたは吸血鬼に身を捧げることが喜びである使徒です。";
				when Vampires.Villages.Vampire_Role =>
					declare
						Mark : constant array(Vampires.Villages.Vampire_Role) of Character := ('K', 'Q', 'J');
					begin
						if Person.Commited and Setting.Target >= 0 then
							return "あなたは吸血鬼(" & Mark(Person.Role) & ")、" & Name(Village.People.Constant_Reference(Setting.Target).Element.all) & "を襲います。";
						else
							return "あなたは吸血鬼(" & Mark(Person.Role) & ")です。";
						end if;
					end;
				when Vampires.Villages.Gremlin =>
					return "あなたは妖魔です。";
			end case;
		end if;
	end Role_Text;
	
	Player_Index : constant Integer := Vampires.Villages.Joined (Village.all, User_Id);
	Message_Counts : Vampires.Villages.Message_Counts renames Vampires.Villages.Count_Messages (Village.all, Day);
	Tip_Showed : Boolean := False;
	
	type Paging_Pos is (Top, Bottom, Tip);
	
	procedure Paging(
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Pos : Paging_Pos)
	is
		Speech_Count : constant Natural := Vampires.Villages.Count_Speech (Village.all, Day);
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
				Link (Object, Output, Village_Id, Day, First => 0, Last => Speech_Count,
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
						Link (Object, Output, Village_Id, Day, First => I_F, Last => I_L,
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
				Link (Object, Output, Village_Id, Day, Latest => Speeches_By_Page,
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
		Tag : in String; Template : in Web.Producers.Template) is
	begin
		if Tag = "userpanel" then
			User_Panel (Object, Output, Template, User_Id, User_Password, False);
		elsif Tag = "stylesheet" then
			Write(Output, "<link rel=""stylesheet"" type=""text/css"" href=");
			Link_Style_Sheet (Object, Output);
			Write(Output, "/>");
		elsif Tag = "background" then
			Link_Image (Object, Output, Object.Configuration.Background_Image_File_Name.all);
		elsif Tag = "styles" then
			if not Village.People.Is_Empty then
				Write(Output, "<style>");
				for I in Village.People.First_Index .. Village.People.Last_Index loop
					Write(Output, ".p" & To_String(I) & "{display:none;} ");
				end loop;
				Write(Output, "</style>");
				for I in Village.People.First_Index .. Village.People.Last_Index loop
					Write(Output, "<style id=""s" & To_String(I) & """>.p" & To_String(I) & "{display:block;} </style>");
				end loop;
			end if;
			-- ここでやるべきでもないがついでに
			if Village.State = Villages.Opened
				and then Village.Day_Duration < 24 * 60 * 60.0
				and then Object.HTML_Version = Web.XHTML
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
						Tag : in String; Template : in Web.Producers.Template) is
					begin
						if Tag = "day" then
							if I /= Day then
								Write(Output, "<a href=");
								Link (Object, Output, Village_Id, I,
									User_Id => User_Id, User_Password => User_Password);
								Write(Output, '>');
							end if;
							Day_Name (Object, Output, I, Village.Today, Village.State);
							if I /= Day then
								Write(Output, "</a>");
							end if;
						else
							Handle_Villages (Output, Tag, Template, Object,
								Village_Id, Village.all, Day, User_Id => User_Id, User_Password => User_Password);
						end if;
					end Handle_Days;
				begin
					Web.Producers.Produce(Output, Template, Handler => Handle_Days'Access);
				end;
			end loop;
		elsif Tag = "person" then
			for I in Village.People.First_Index .. Village.People.Last_Index loop
				declare
					Person : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(I).Element.all;
					procedure Handle_Summary(Output : not null access Ada.Streams.Root_Stream_Type'Class;
						Tag : in String; Template : in Web.Producers.Template) is
					begin
						if Tag = "name" then
							Write(Output,
								"<label for=""c" & To_String(I) & """>" &
								"<input id=""c" & To_String(I) & """ type=""checkbox"" checked=""checked"" onClick=""javascript:sync(" & To_String(I) & ")"" />");
							Web.Write_In_HTML (Output, Object.HTML_Version, Name (Person));
							Write (Output, "</label>");
						elsif Tag = "speech" then
							Write(Output, To_String(Message_Counts(I).Speech));
							if Message_Counts(I).Encouraged > 0 then
								Write(Output, " <small>/");
								Write(Output, To_String(Speech_Limit + Message_Counts(I).Encouraged * Encouraged_Speech_Limit));
								Write(Output, "</small>");
							end if;
						elsif Tag = "administrator" then
							if User_id = Tabula.Users.Administrator then
								Web.Producers.Produce(Output, Template, Handler => Handle_Summary'Access);
							end if;
						elsif Tag = "id" then
							Web.Write_In_HTML (Output, Object.HTML_Version, +Person.Id);
						elsif Tag = "remove" then
							if Village.State = Villages.Prologue then
								Web.Producers.Produce(Output, Template, Handler => Handle_Summary'Access);
							end if;
						elsif Tag = "htarget" then
							Write(Output, "<input type=""hidden"" name=""target"" value=""");
							Write(Output, To_String(I));
							Write(Output, """/>");
						else
							Handle_Villages (Output, Tag, Template, Object,
								Village_Id, Village.all, Day, User_Id => User_Id, User_Password => User_Password);
						end if;
					end Handle_Summary;
				begin
					if (Village.State >= Villages.Epilogue and then Village.Today = Day)
						or else Person.Records.Constant_Reference(Day).Element.State /= Vampires.Villages.Died
					then
						Web.Producers.Produce(Output, Template, Handler => Handle_Summary'Access);
					end if;
				end;
			end loop;
		elsif Tag = "back" then
			Write(Output, "<a href=");
			Link (Object, Output, User_Id => User_Id, User_Password => User_Password);
			Write(Output, '>');
			Web.Producers.Produce(Output, Template);
			Write(Output, "</a>");
		elsif Tag = "message" then
			declare
				procedure Narration(Message : String; Class : String := "narration") is
					procedure Handle_Narration(Output : not null access Ada.Streams.Root_Stream_Type'Class;
						Tag : in String; Template : in Web.Producers.Template) is
					begin
						Web.Write_In_HTML (Output, Object.HTML_Version, Message);
					end Handle_Narration;
				begin
					Web.Producers.Produce(Output, Template, Class, Handler => Handle_Narration'Access);
				end Narration;
				procedure Speech(Message : Vampires.Villages.Message; Class : String; Time : Ada.Calendar.Time) is
					procedure Handle_Speech(Output : not null access Ada.Streams.Root_Stream_Type'Class;
						Tag : in String; Template : in Web.Producers.Template) is
					begin
						Handle_Messages (Output, Tag, Template, Object,
							Village_Id, Village.all, Day, Message, Time, User_Id => User_Id, User_Password => User_Password);
					end Handle_Speech;
				begin
					Web.Producers.Produce(Output, Template, Class, Handler => Handle_Speech'Access);
				end Speech;
				procedure Note(Subject : Vampires.Villages.Person_Type; Note : Vampires.Villages.Person_Record; Class : String) is
					procedure Handle_Note(Output : not null access Ada.Streams.Root_Stream_Type'Class;
						Tag : in String; Template : in Web.Producers.Template) is
					begin
						if Tag = "image" then
							Link_Image (Object, Output, +Subject.Image);
						elsif Tag = "name" then
							Write(Output, Name(Subject));
						elsif Tag = "text" then
							declare
								S : String renames Ada.Strings.Unbounded.To_String(Note.Note);
							begin
								if S = "" then
									Write(Output, "……。");
								else
									Web.Write_In_HTML (Output, Object.HTML_Version, S);
								end if;
							end;
						else
							Handle_Villages (Output, Tag, Template, Object,
								Village_Id, Village.all, Day, User_Id => User_Id, User_Password => User_Password);
						end if;
					end Handle_Note;
				begin
					Web.Producers.Produce(Output, Template, Class, Handler => Handle_Note'Access);
				end Note;
				subtype X_Type is Integer range 1 .. 3;
				package Random_X is new Ada.Numerics.MT19937.Discrete_Random(X_Type);
				Executed : Integer := -1;
				Speech_Count : Natural := 0;
				X : X_Type := 2;
				Last_Speech : Integer := -1;
				Last_Speech_Time : Ada.Calendar.Time := Calendar.Null_Time;
				X_Generator : aliased Ada.Numerics.MT19937.Generator;
			begin
				Ada.Numerics.MT19937.Reset(X_Generator, 12);
				if Object.HTML_Version = Web.HTML then
					Paging (Output, Top);
				end if;
				for Position in Village.Messages.First_Index .. Village.Messages.Last_Index loop
					declare
						Message : Vampires.Villages.Message renames Village.Messages.Constant_Reference(Position).Element.all;
					begin
						if Message.Day = Day then
							if (First < 0 or else First <= Speech_Count) and then (Last < 0 or else Speech_Count <= Last) then
								case Message.Kind is
									when Vampires.Villages.Narration =>
										Narration(+Message.Text);
									when Vampires.Villages.Escape =>
										declare
											Subject : Vampires.Villages.Person_Type renames Village.Escaped_People.Constant_Reference(Message.Subject).Element.all;
										begin
											if Village.State >= Tabula.Villages.Epilogue then
												Narration(Name(Subject) & "(" & (+Subject.Id) & ")は人知れず華やいだ都会へと旅立ってゆきました。", Class => "narratione");
											else
												Narration(Name(Subject) & "は人知れず華やいだ都会へと旅立ってゆきました。", Class => "narratione");
											end if;
										end;
									when Vampires.Villages.Join =>
										declare
											Subject : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(Message.Subject).Element.all;
										begin
											Narration(To_String(Message.Subject + 1) & "人目に" & Name(Subject) & "が現れました。");
										end;
									when Vampires.Villages.Escaped_Join =>
										declare
											Subject : Vampires.Villages.Person_Type renames Village.Escaped_People.Constant_Reference(Message.Subject).Element.all;
										begin
											Narration(Name(Subject) & "が現れました。", Class => "narratione");
										end;
									when Vampires.Villages.Speech | Vampires.Villages.Escaped_Speech =>
										if Message.Kind = Vampires.Villages.Speech
											or else Vampires.Villages.Rejoined (Village.all, Message.Subject) >= 0
										then
											declare
												Subject : Integer;
											begin
												if Message.Kind = Vampires.Villages.Speech then
													Subject := Message.Subject;
												else
													Subject := Vampires.Villages.Rejoined (Village.all, Message.Subject);
												end if;
												if Last_Speech /= Subject then
													New_X : loop
														declare
															X2 : constant X_Type := Random_X.Random(X_Generator'Access);
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
											if Object.HTML_Version = Web.XHTML then
												Write(Output, "<div class=""s");
												Write(Output, To_String(X));
												Write(Output, " p");
												Write(Output, To_String(Last_Speech));
												Write(Output, """>");
											end if;
											Speech (Message, "speech", Last_Speech_Time);
											if Object.HTML_Version = Web.XHTML then
												Write(Output, "</div>");
											end if;
										else
											Speech(Message, "escaped", Message.Time);
										end if;
									when Vampires.Villages.Monologue =>
										if Village.State >= Villages.Epilogue
											or else Message.Subject = Player_Index
										then
											Speech(Message, "monologue", Message.Time);
										end if;
									when Vampires.Villages.Ghost =>
										if Village.State >= Villages.Epilogue
											or else (Player_Index >= 0 and then Village.People.Constant_Reference(Player_Index).Element.Records.Constant_Reference(Village.Today).Element.State = Vampires.Villages.Died)
										then
											Speech(Message, "ghost", Message.Time);
										end if;
									when Vampires.Villages.Howling =>
										if Village.State >= Villages.Epilogue
											or else (Player_Index >= 0 and then Village.People.Constant_Reference(Player_Index).Element.Role in Vampires.Villages.Vampire_Role)
										then
											Speech(Message, "vampire", Message.Time);
										end if;
									when Vampires.Villages.Howling_Blocked =>
										if Village.State >= Villages.Epilogue
											or else (Player_Index >= 0 and then Village.People.Constant_Reference(Player_Index).Element.Role in Vampires.Villages.Vampire_Role)
										then
											declare
												The_Unfortunate : constant Integer := Vampires.Villages.Find_Superman (Village.all, Vampires.Villages.Unfortunate_Inhabitant);
											begin
												Narration(Name(Village.People.Constant_Reference(The_Unfortunate).Element.all) & "のせいで用事ができてしまい、今夜は相談ができません。", "narrationi");
											end;
										end if;
									when Vampires.Villages.Action_Message_Kind =>
										declare
											Subject : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(Message.Subject).Element.all;
											Target : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(Message.Target).Element.all;
										begin
											case Vampires.Villages.Action_Message_Kind(Message.Kind) is
												when Vampires.Villages.Action_Wake =>
													Narration(Name(Subject) & "は" & Name(Target) & "を起こした。");
												when Vampires.Villages.Action_Encourage =>
													Narration(Name(Subject) & "は" & Name(Target) & "に話の続きを促した。");
												when Vampires.Villages.Action_Vampire_Gaze =>
													if Village.State >= Villages.Epilogue
														or else (Player_Index >= 0 and then Village.People.Constant_Reference(Player_Index).Element.Role in Vampires.Villages.Vampire_Role)
													then
														Narration(Name(Subject) & "は" & Name(Target) & "をこっそりと見つめた。", "narrationi");
													end if;
												when Vampires.Villages.Action_Vampire_Gaze_Blocked =>
													if Village.State >= Villages.Epilogue
														or else (Player_Index >= 0 and then Village.People.Constant_Reference(Player_Index).Element.Role in Vampires.Villages.Vampire_Role)
													then
														declare
															The_Unfortunate : constant Integer := Vampires.Villages.Find_Superman (Village.all, Vampires.Villages.Unfortunate_Inhabitant);
														begin
															Narration(Name(Subject) & "の視線は" & Name(Village.People.Constant_Reference(The_Unfortunate).Element.all) & "に遮られた。", "narrationi");
														end;
													end if;
											end case;
										end;
									when Vampires.Villages.Servant_Message_Kind =>
										if Village.State >= Villages.Epilogue or else Player_Index = Message.Subject then
											Narration (Servant_Knew_Message (Village.all, Message), "narrationi");
										end if;
									when Vampires.Villages.Doctor_Message_Kind =>
										if Village.State >= Villages.Epilogue or else (Player_Index = Message.Subject) then
											Narration (Doctor_Cure_Message (Village.all, Message), "narrationi");
										end if;
									when Vampires.Villages.Detective_Message_Kind =>
										if Village.State >= Villages.Epilogue or else (Player_Index = Message.Subject) then
											Narration (Detective_Survey_Message (Village.all, Message), "narrationi");
											if Message.Text /= Ada.Strings.Unbounded.Null_Unbounded_String
												and then (
													Village.Daytime_Preview = Vampires.Villages.Role_And_Message
													or else Village.Daytime_Preview = Vampires.Villages.Message_Only
													or else Message.Kind /= Vampires.Villages.Detective_Survey_Preview)
											then
												Speech(Message, "dying", Message.Time);
											end if;
										end if;
									when Vampires.Villages.Provisional_Vote =>
										Narration (Vote_Report (Village.all, Day => Message.Day, Provisional => True, Player_Index => -1));
										Narration (Vote_Count (Village.all, Day => Message.Day, Provisional => True, Executed => -1));
									when Vampires.Villages.Execution =>
										Narration (Vote_Report (Village.all, Day => Message.Day - 1, Provisional => False, Player_Index => Player_Index), "narrationi");
										Narration (Vote_Count (Village.all, Day => Message.Day - 1, Provisional => False, Executed => Message.Target));
										Executed := Message.Target;
									when Vampires.Villages.Awareness =>
										if Village.State >= Villages.Epilogue or else Player_Index = Message.Subject then
											declare
												Subject : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(Message.Subject).Element.all;
											begin
												Narration(Name(Subject) & "は無性に闇が恋しくなり……夜空へと飛び立ちました。", "narrationi");
											end;
										end if;
									when Vampires.Villages.Astronomer_Observation =>
										if Village.State >= Villages.Epilogue or else Player_Index = Message.Subject then
											Narration (Astronomer_Observation_Message (Village.all, Message), "narrationi");
										end if;
									when Vampires.Villages.Hunter_Message_Kind =>
										if Village.State >= Tabula.Villages.Epilogue or else Player_Index = Message.Subject then
											Narration (Hunter_Guard_Message (Village.all, Message), "narrationi");
										end if;
									when Vampires.Villages.Meeting => null;
										if Village.State >= Villages.Epilogue
											or else (Player_Index >= 0 and then Village.People.Constant_Reference(Player_Index).Element.Role in Vampires.Villages.Vampire_Role)
										then
											for Role in Vampires.Villages.Vampire_Role loop
												for I in Village.People.First_Index .. Village.People.Last_Index loop
													declare
														Person : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(I).Element.all;
													begin
														if Person.Role = Role then
															declare
																Yesterday_Record : Vampires.Villages.Person_Record renames Person.Records.Constant_Reference(Day - 1).Element.all;
															begin
																if Yesterday_Record.State /= Vampires.Villages.Died and then Executed /= I then
																	Note(Person, Yesterday_Record, "vampire");
																end if;
															end;
														end if;
													end;
												end loop;
											end loop;
										end if;
									when Vampires.Villages.Vampire_Message_Kind =>
										if Village.State >= Villages.Epilogue or else (Player_Index >= 0 and then (Message.Subject = Player_Index or else (
											Village.People.Constant_Reference(Message.Subject).Element.Role in Vampires.Villages.Vampire_Role
											and then Village.People.Constant_Reference(Player_Index).Element.Role in Vampires.Villages.Vampire_Role)))
										then
											Narration (Vampire_Murder_Message (Village.all, Message, Executed), "narrationi");
										end if;
									when Vampires.Villages.Gremlin_Sense =>
										if Village.State >= Villages.Epilogue or else (Player_Index = Message.Subject) then
											declare
												Vampire_Count : Natural := 0;
											begin
												for I in Village.People.First_Index .. Village.People.Last_Index loop
													declare
														P : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(I).Element.all;
														R : Vampires.Villages.Person_Record renames P.Records.Constant_Reference(Message.Day).Element.all;
													begin
														if (R.State = Vampires.Villages.Normal and then P.Role in Vampires.Villages.Vampire_Role)
															or else R.State = Vampires.Villages.Infected
														then
															Vampire_Count := Vampire_Count + 1;
														end if;
													end;
												end loop;
												Narration("残り吸血鬼の数は" & To_String(Vampire_Count) & "匹……。", "narrationi");
											end;
										end if;
									when Vampires.Villages.Gremlin_Killed =>
										if Village.State >= Villages.Epilogue or else (Player_Index = Message.Subject) then
											Narration("妖魔は滅びました。", "narrationi");
										end if;
									when Vampires.Villages.Sweetheart_Incongruity =>
										if Village.State >= Villages.Epilogue or else (Player_Index = Message.Subject) then
											declare
												S : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(Message.Subject).Element.all;
												T : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(Message.Target).Element.all;
											begin
												Narration(Name(S) & "は" & Name(T) & "に違和感を感じました。", "narrationi");
											end;
										end if;
									when Vampires.Villages.Sweetheart_Suicide =>
										if Village.State >= Villages.Epilogue or else (Player_Index = Message.Subject) or else (Player_Index = Message.Target) then
											declare
												S : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(Message.Subject).Element.all;
											begin
												Narration(Name(S) & "は想い人の後を追いました。", "narrationi");
											end;
										end if;
									when Vampires.Villages.List =>
										declare
											Log : Ada.Strings.Unbounded.Unbounded_String;
										begin
											if Village.Today = Message.Day and Village.State >= Villages.Epilogue then
												declare
													S : String renames Fatalities_List (Village.all, Message.Day, Executed);
												begin
													if S /= "" then
														Narration(S);
													end if;
												end;
												declare
													Last_Day_Messages : Vampires.Villages.Message_Counts renames Vampires.Villages.Count_Messages (Village.all, Message.Day - 1);
													G_Win : Boolean := False;
													V_Win : Boolean := False;
													Second : Boolean := False;
												begin
													for I in Village.People.First_Index .. Village.People.Last_Index loop
														declare
															P : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(I).Element.all;
															Speech_Count : constant Natural := Last_Day_Messages(I).Speech;
														begin
															if Second then
																Ada.Strings.Unbounded.Append(Log, Line_Break);
															end if;
															Ada.Strings.Unbounded.Append(Log, Name(P) & "(" & P.Id & ")は" & Image(P.Role) & "でした。");
															case P.Records.Constant_Reference(Message.Day).Element.State is
																when Vampires.Villages.Normal =>
																	Ada.Strings.Unbounded.Append(Log, "生存しました。");
																	case P.Role is
																		when Vampires.Villages.Vampire_Role =>
																			if Speech_Count > 0 then
																				V_Win := True;
																			end if;
																		when Vampires.Villages.Gremlin =>
																			if Speech_Count > 0 then
																				G_Win := True;
																			end if;
																		when others => null;
																	end case;
																	if Speech_Count = 0 then
																		Ada.Strings.Unbounded.Append(Log, "蚊帳の外でした。");
																	end if;
																when Vampires.Villages.Infected =>
																	Ada.Strings.Unbounded.Append(Log, "吸血鬼にされました。");
																when Vampires.Villages.Died =>
																	Ada.Strings.Unbounded.Append(Log, "死亡しました。");
															end case;
															Second := True;
														end;
													end loop;
													Narration(+Log);
													Log := Ada.Strings.Unbounded.Null_Unbounded_String;
													if G_Win then
														if V_Win then
															Ada.Strings.Unbounded.Append(Log, "村は吸血鬼の手に落ちた……流石の吸血鬼も安堵したその油断を突き……妖魔は全てに勝利しました。");
														else
															Ada.Strings.Unbounded.Append(Log, "吸血鬼を退治した……その瞬間、彼奴を阻むものは何もなくなり……妖魔は全てに勝利しました。");
														end if;
													elsif V_Win then
														Ada.Strings.Unbounded.Append(Log, "村は吸血鬼の手に落ちました。吸血鬼の勝利です！");
													else
														Ada.Strings.Unbounded.Append(Log, "吸血鬼を退治しました。村人の勝利です！");
													end if;
												end;
												Narration (+Log);
											else
												declare
													S : String renames Fatalities_List (Village.all, Message.Day, Executed);
												begin
													Ada.Strings.Unbounded.Append(Log, S);
													if S /= "" then
														Ada.Strings.Unbounded.Append(Log, Line_Break);
													end if;
												end;
												Ada.Strings.Unbounded.Append (Log, Survivors_List (Village.all, Message.Day));
												Narration (+Log);
												if Message.Day = 2 then
													case Village.Execution is
														when Vampires.Villages.From_Second | Vampires.Villages.Provisional_Voting_From_Second =>
															Narration (For_Execution_Message);
														when others =>
															null;
													end case;
												end if;
											end if;
										end;
									when Vampires.Villages.Introduction =>
										Narration (Stages (Stage (Village.all)).Introduction.all);
									when Vampires.Villages.Breakdown =>
										if Village.State >= Villages.Epilogue
											or else (Player_Index >= 0 and then Village.People.Constant_Reference(Player_Index).Element.Role in Vampires.Villages.Vampire_Role)
										then
											Narration (Vampires_List (Village.all), "narrationi");
										end if;
										case Village.Execution is
											when Vampires.Villages.From_Second | Vampires.Villages.Provisional_Voting_From_Second =>
												Narration (Stages (Stage (Village.all)).Breakdown.all);
											when Vampires.Villages.Dummy_Killed_And_From_First | Vampires.Villages.From_First =>
												Narration (Stages (Stage (Village.all)).Breakdown.all & Line_Break & For_Execution_Message);
										end case;
										Narration (Breakdown_List (Village.all));
								end case;
							end if;
							if Message.Kind = Vampires.Villages.Speech or else Message.Kind = Vampires.Villages.Escaped_Speech then
								Speech_Count := Speech_Count + 1;
							end if;
						end if;
					end;
				end loop;
				if Last < 0 or else Speech_Count <= Last + 1 then
					Tip_Showed := True;
					if Village.State >= Villages.Epilogue and then Day < Village.Today then
						for I in Village.People.First_Index .. Village.People.Last_Index loop
							declare
								Subject : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(I).Element.all;
								Rec : Vampires.Villages.Person_Record renames Subject.Records.Constant_Reference(Day).Element.all;
							begin
								if Rec.State = Vampires.Villages.Died and then Rec.Note /= Ada.Strings.Unbounded.Null_Unbounded_String then
									Note(Subject, Rec, "dying");
								end if;
							end;
						end loop;
					end if;
					if Day = Village.Today and then Village.State /= Villages.Closed then
						declare
							procedure Handle_Guidance(Output : not null access Ada.Streams.Root_Stream_Type'Class;
								Tag : in String; Template : in Web.Producers.Template) is
							begin
								if Village.State <= Villages.Opened then
									declare
										Second : Boolean := False;
									begin
										for I in Village.People.First_Index .. Village.People.Last_Index loop
											declare
												P : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(I).Element.all;
											begin
												if not P.Commited and then P.Records.Constant_Reference(Village.Today).Element.State /= Vampires.Villages.Died then
													if Second then
														Write(Output, "、");
													end if;
													Write(Output, Name(P));
													Second := True;
												end if;
											end;
										end loop;
										if Second then
											Write(Output, "が行動しています。");
										end if;
									end;
								end if;
								case Village.State is
									when Villages.Prologue =>
										if Village.People.Length < Minimum_Number_Of_Persons then
											Write(Output, To_String(Minimum_Number_Of_Persons));
											Write(Output, "人以上の参加を待っています。");
										else
											Write(Output, "全員が行動を終えると夜が明けます。");
										end if;
									when Villages.Opened =>
										case Village.Time is
											when Villages.Daytime =>
												if Village.Today >= 2 and then
													Village.Execution = Vampires.Villages.Provisional_Voting_From_Second and then
													not Village.Provisional_Voted
												then
													Write (Output, Ada.Calendar.Formatting.Image (Village.Dawn + Village.Day_Duration / 2, Time_Zone => Calendar.Time_Offset));
													Write (Output, "に一次開票します。");
												end if;
												Write (Output, Ada.Calendar.Formatting.Image (Village.Dawn + Village.Day_Duration, Time_Zone => Calendar.Time_Offset));
												Write (Output, "までに行動を終えてください。");
											when Villages.Vote =>
												Write(Output, "全員の投票を待っています。");
											when Villages.Night =>
												Write(Output, "夜です。");
										end case;
										if Village.Day_Duration < 24 * 60 * 60.0
											and then Object.HTML_Version = Web.XHTML
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
							Web.Producers.Produce(Output, Template, "narration", Handler => Handle_Guidance'Access);
						end;
					end if;
					if Object.HTML_Version = Web.HTML then
						Paging (Output, Village_Page.Tip);
					end if;
				else
					if Object.HTML_Version = Web.HTML then
						Paging (Output, Bottom);
					end if;
				end if;
			end;
		elsif Tag = "villagepanel" then
			if ((Player_Index >= 0 or else User_Id = Tabula.Users.Administrator) and Village.Today = Day)
				or else (User_Id /= "" and Village.State = Villages.Prologue)
			then
				if Village.State = Villages.Closed then
					Web.Producers.Produce(Output, Template, "closed");
				elsif Player_Index >= 0 then
					declare
						Person : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(Player_Index).Element.all;
						Bottom : Boolean := True;
						procedure Handle_Player(Output : not null access Ada.Streams.Root_Stream_Type'Class;
							Tag : in String; Template : in Web.Producers.Template) is
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
									and then Village.People.Constant_Reference(Player_Index).Element.Records.Constant_Reference(Village.Today).Element.State /= Vampires.Villages.Died
									and then not Person.Commited)
								then
									if Village.State = Villages.Opened then
										declare
											Rest : constant Integer := Speech_Limit + Message_Counts(Player_Index).Encouraged * Encouraged_Speech_Limit - Message_Counts(Player_Index).Speech;
											procedure Handle_Speech(Output : not null access Ada.Streams.Root_Stream_Type'Class;
												Tag : in String; Template : in Web.Producers.Template) is
											begin
												if Tag = "count" then
													Write(Output, To_String(Rest));
												elsif Tag = "rest" then
													Web.Producers.Produce(Output, Template, Handler => Handle_Speech'Access);
												else
													Handle_Player(Output, Tag, Template);
												end if;
											end Handle_Speech;
										begin
											if Rest > 0 then
												Web.Producers.Produce(Output, Template, Handler => Handle_Speech'Access);
											end if;
										end;
									else
										Web.Producers.Produce(Output, Template, Handler => Handle_Player'Access);
									end if;
								end if;
							elsif Tag = "monologue" then
								if Village.State = Villages.Opened
									and then Village.People.Constant_Reference(Player_Index).Element.Records.Constant_Reference(Village.Today).Element.State /= Vampires.Villages.Died
									and then not Person.Commited
								then
									declare
										Rest : constant Integer := Monologue_Limit - Message_Counts(Player_Index).Monologue;
										procedure Handle_Monologue(Output : not null access Ada.Streams.Root_Stream_Type'Class;
											Tag : in String; Template : in Web.Producers.Template) is
										begin
											if Tag = "count" then
												Write(Output, To_String(Rest));
											elsif Tag = "rest" then
												Web.Producers.Produce(Output, Template, Handler => Handle_Monologue'Access);
											else
												Handle_Player(Output, Tag, Template);
											end if;
										end Handle_Monologue;
									begin
										if Rest > 0 then
											Web.Producers.Produce(Output, Template, Handler => Handle_Monologue'Access);
										end if;
									end;
								end if;
							elsif Tag ="ghost" then
								if Village.State = Villages.Opened
									and then Village.People.Constant_Reference(Player_Index).Element.Records.Constant_Reference(Village.Today).Element.State = Vampires.Villages.Died
								then
									declare
										Rest : constant Integer := Ghost_Limit - Message_Counts(Player_Index).Ghost;
										procedure Handle_Ghost(Output : not null access Ada.Streams.Root_Stream_Type'Class;
											Tag : in String; Template : in Web.Producers.Template) is
										begin
											if Tag = "count" then
												Write(Output, To_String(Rest));
											elsif Tag = "rest" then
												Web.Producers.Produce(Output, Template, Handler => Handle_Ghost'Access);
											else
												Handle_Player(Output, Tag, Template);
											end if;
										end Handle_Ghost;
									begin
										if Rest > 0 then
											Web.Producers.Produce(Output, Template, Handler => Handle_Ghost'Access);
										end if;
									end;
								end if;
							elsif Tag ="vampire" then
								if Village.State = Villages.Opened
									and then not Person.Commited
									and then Person.Role in Vampires.Villages.Vampire_Role
									and then (Message_Counts(Player_Index).Speech > 0 or else Village.Time = Villages.Night)
								then
									Web.Producers.Produce(Output, Template, Handler => Handle_Player'Access);
								end if;
							elsif Tag ="dying" then
								if Village.State = Villages.Opened
									and then not Person.Commited
									and then Village.People.Constant_Reference(Player_Index).Element.Records.Constant_Reference(Village.Today).Element.State = Vampires.Villages.Died
								then
									Web.Producers.Produce(Output, Template, Handler => Handle_Player'Access);
								end if;
							elsif Tag = "note" then
								Write(Output, +Person.Records.Constant_Reference(Target_Day).Element.Note);
							elsif Tag = "rest" then
								null;
							elsif Tag = "zero" then
								if Village.State = Villages.Opened
									and then Village.Time /= Villages.Night
									and then not Village.People.Constant_Reference(Player_Index).Element.Commited
									and then Village.People.Constant_Reference(Player_Index).Element.Records.Constant_Reference(Village.Today).Element.State /= Vampires.Villages.Died
									and then Message_Counts(Player_Index).Speech = 0
								then
									Web.Producers.Produce(Output, Template);
								end if;
							elsif Tag = "role" then
								if Village.State = Villages.Opened then
									if Village.People.Constant_Reference(Player_Index).Element.Records.Constant_Reference(Village.Today).Element.State /= Vampires.Villages.Died then
										Web.Producers.Produce(Output, Template, Handler => Handle_Player'Access);
									else
										Write(Output, Role_Text(Person));
									end if;
								end if;
							elsif Tag = "roletext" then
								Write(Output, Role_Text(Person));
							elsif Tag = "roleimg" then
								if Village.People.Constant_Reference(Player_Index).Element.Records.Constant_Reference(Village.Today).Element.State /= Vampires.Villages.Died then
									Link_Image (Object, Output, Role_Image_File_Name(Person.Role).all);
								end if;
							elsif Tag = "vote" then
								if Village.State = Villages.Opened
									and then Message_Counts(Player_Index).Speech > 0
									and then (Village.Execution = Vampires.Villages.From_First or else Village.Execution = Vampires.Villages.Dummy_Killed_And_From_First or else Village.Today /= 1)
								then
									if Person.Commited then
										declare
											Setting : Vampires.Villages.Person_Record renames Person.Records.Constant_Reference(Village.Today).Element.all;
										begin
											if Setting.Vote < 0 then
												Write(Output, "<div>処刑を選ぶ投票は棄権します。</div>");
											else
												Write(Output, "<div>処刑には");
												declare
													Target : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(Setting.Vote).Element.all;
												begin
													Write(Output, Name(Target));
												end;
												Write(Output, "を推しています。</div>");
											end if;
										end;
									else
										Vote_Form (Output, Player_Index, Vampires.Villages.Inhabitant, 
											Special => False,
											Current => Person.Records.Constant_Reference(Village.Today).Element.Vote,
											Current_Special => False,
											Message => "誰を処刑に……",
											Button => "投票");
									end if;
								end if;
							elsif Tag = "ability" then
								if Village.State = Villages.Opened
									and then not Person.Commited
									and then (Message_Counts(Player_Index).Speech > 0 or else Village.Time = Villages.Night)
								then
									case Person.Role is
										when Vampires.Villages.Inhabitant | Vampires.Villages.Loved_Inhabitant | Vampires.Villages.Unfortunate_Inhabitant
											| Vampires.Villages.Lover | Vampires.Villages.Sweetheart_M | Vampires.Villages.Sweetheart_F
											| Vampires.Villages.Servant | Vampires.Villages.Gremlin => null;
										when Vampires.Villages.Doctor =>
											if Village.People.Constant_Reference(Player_Index).Element.Records.Constant_Reference(Village.Today).Element.Target < 0 then
												if Village.Today >= 2 then
													Vote_Form (Output, Player_Index, Vampires.Villages.Doctor, False,
														Person.Records.Constant_Reference(Village.Today).Element.Target, False, "貴重な薬を誰に……", "診察");
												else
													Write(Output, "<div>今は他に犠牲者がいないと信じましょう。</div>");
												end if;
											end if;
										when Vampires.Villages.Detective =>
											if Village.People.Constant_Reference(Player_Index).Element.Records.Constant_Reference(Village.Today).Element.Target < 0 then
												for I in Village.People.First_Index .. Village.People.Last_Index loop
													if Village.People.Constant_Reference(I).Element.Records.Constant_Reference(Village.Today).Element.State = Vampires.Villages.Died then
														Vote_Form (Output, Player_Index, Vampires.Villages.Detective, False,
															Person.Records.Constant_Reference(Village.Today).Element.Target, False, "どの被害者を調べますか……", "調査");
														goto Exit_Detective_Target;
													end if;
												end loop;
												if Village.Execution = Vampires.Villages.Dummy_Killed_And_From_First and Day <= 1 then
													Write(Output, "<div>地主さんを調査しています。</div>");
												else
													Write(Output, "<div>まだ村人に被害者はいません。</div>");
												end if;
												<<Exit_Detective_Target>> null;
											end if;
										when Vampires.Villages.Astronomer =>
											Vote_Form (Output, Player_Index, Vampires.Villages.Astronomer, False,
												Person.Records.Constant_Reference(Target_Day).Element.Target, False, "どの家の上空の星が奇麗……", "観測");
										when Vampires.Villages.Hunter =>
											declare
												Has_Silver_Bullet : Boolean := True;
											begin
												for I in 0 .. Target_Day - 1 loop
													if Person.Records.Constant_Reference(I).Element.Special then
														Has_Silver_Bullet := False;
													end if;
												end loop;
												Vote_Form (Output, Player_Index, Vampires.Villages.Hunter, Has_Silver_Bullet,
													Person.Records.Constant_Reference(Target_Day).Element.Target, Person.Records.Constant_Reference(Target_Day).Element.Special, "誰を守りますか……", "護衛");
											end;
										when Vampires.Villages.Vampire_Role =>
											Vote_Form (Output, Player_Index, Vampires.Villages.Vampire_K, False,
												Person.Records.Constant_Reference(Target_Day).Element.Target, False, "誰の血が旨そうでしょうか……", "襲撃");
									end case;
								end if;
							elsif Tag = "action" then
								if Village.State = Villages.Opened and then (
									Message_Counts(Player_Index).Wake = 0
									or else Message_Counts(Player_Index).Encourage = 0
									or else ((Village.People.Constant_Reference(Player_Index).Element.Role in Vampires.Villages.Vampire_Role)
										and then Message_Counts(Player_Index).Vampire_Gaze = 0))
								then
									if Object.HTML_Version = Web.XHTML then
										Write(Output, "<form method=""POST"" class=""inner"">" & Line_Break);
									else
										Write(Output, "<form method=""POST"" action=");
										Link (Object, Output, Village_Id => Village_Id,
											User_Id => User_Id, User_Password => User_Password);
										Write(Output, ">");
									end if;
									Write(Output, "<select name=""target"">" & Line_Break);
									Write(Output, "<option value=""-1"" selected=""selected""></option>" & Line_Break);
									for I in Village.People.First_Index .. Village.People.Last_Index loop
										if I /= Player_Index
											and then Village.People.Constant_Reference(I).Element.Records.Constant_Reference(Village.Today).Element.State /= Vampires.Villages.Died
										then
											declare
												Person : Vampires.Villages.Person_Type renames Village.People.Constant_Reference(I).Element.all;
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
									if Village.People.Constant_Reference(Player_Index).Element.Role in Vampires.Villages.Vampire_Role
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
										Setting : Vampires.Villages.Person_Record renames Person.Records.Constant_Reference(Village.Today).Element.all;
									begin
										if Setting.State /= Vampires.Villages.Died then
											Web.Producers.Produce(Output, Template, Handler => Handle_Player'Access);
										end if;
									end;
								end if;
							elsif Tag = "escape" then
								if Village.State = Villages.Prologue then
									declare
										subtype Arg is Integer range 1000 .. 4999;
										package Random_Arg is new Ada.Numerics.MT19937.Discrete_Random(Arg);
										Seed : aliased Ada.Numerics.MT19937.Generator;
										X : Arg;
										Y : Arg;
										procedure Handle_Escape(Output : not null access Ada.Streams.Root_Stream_Type'Class;
											Tag : in String; Template : in Web.Producers.Template) is
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
										Ada.Numerics.MT19937.Reset(Seed);
										X := Random_Arg.Random(Seed'Access);
										Y := Random_Arg.Random(Seed'Access);
										Web.Producers.Produce(Output, Template, Handler => Handle_Escape'Access);
									end;
								end if;
							elsif Tag = "commited" then
								if Person.Commited then
									Web.Producers.Produce(Output, Template, Handler => Handle_Player'Access);
								end if;
							else
								Handle_Villages (Output, Tag, Template, Object,
									Village_Id, Village.all, Day, User_Id => User_Id, User_Password => User_Password);
							end if;
						end Handle_Player;
					begin
						Web.Producers.Produce(Output, Template, "player", Handler => Handle_Player'Access);
					end;
				elsif User_Id = Tabula.Users.Administrator then
					Web.Producers.Produce(Output, Template, "administrator", Handler=> Handle'Access);
				elsif Village.State > Villages.Prologue then
					Web.Producers.Produce(Output, Template, "opened");
				elsif Village.People.Length >= Tabula.Maximum_Number_Of_Persons then
					Web.Producers.Produce(Output, Template, "over");
				else
					declare
						Cast : Casts.Cast_Collection := Casts.Load;
						procedure Handle_Entry(Output : not null access Ada.Streams.Root_Stream_Type'Class;
							Tag : in String; Template : in Web.Producers.Template) is
						begin
							if Tag = "works" then
								Write(Output, "<select id=""work"" name=""work"">" &
									"<option value=""-1"" selected=""selected"">(既定)</option>");
								for Position in Cast.Works.First_Index .. Cast.Works.Last_Index loop
									declare
										Item : Casts.Work renames Cast.Works.Constant_Reference(Position).Element.all;
									begin
										if not Casts.Is_Empty (Item) then
											Write(Output, "<option value=""");
											Write(Output, To_String(Position));
											Write(Output, """>");
											Write(Output, +Item.Name);
											case Item.Sex is
												when Casts.Male => Write(Output, " (男性職)");
												when Casts.Female => Write(Output, " (女性職)");
												when Casts.Neutral => null;
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
									type Sex_To_String is array(Casts.Person_Sex) of String(1 .. 9);
									Sex_Name : constant Sex_To_String := (" (男性)", " (女性)");
								begin
									for Position in Cast.People.First_Index .. Cast.People.Last_Index loop
										declare
											Item : Casts.Person renames Cast.People.Constant_Reference(Position).Element.all;
										begin
											if not Casts.Is_Empty (Item) then
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
								for I in Vampires.Villages.Requested_Role loop
									Write(Output, "<option value=""");
									Write(Output, Vampires.Villages.Requested_Role'Image(I));
									Write(Output, """>");
									Write(Output, Image(I));
									Write(Output, "</option>");
								end loop;
								Write(Output, "</select>");
							else
								Handle_Villages (Output, Tag, Template, Object,
									Village_Id, Village.all, Day, User_Id => User_Id, User_Password => User_Password);
							end if;
						end Handle_Entry;
					begin
						Vampires.Villages.Exclude_Taken (Cast, Village.all);
						Web.Producers.Produce(Output, Template, "entry", Handler => Handle_Entry'Access);
					end;
				end if;
			end if;
		elsif Tag = "rule" then
			if Day = 0 and then Tip_Showed then
				Rule_Panel (
					Output => Output,
					Template => Template,
					Village_Id => Village_Id,
					Village => Village,
					Player => Player_Index >= 0,
					User_Id => User_Id,
					User_Password => User_Password);
			end if;
		elsif Tag = "next" then
			if Day < Village.Today and then Tip_Showed then
				declare
					procedure Handle_Next(Output : not null access Ada.Streams.Root_Stream_Type'Class;
						Tag : in String; Template : in Web.Producers.Template) is
					begin
						if Tag = "uri" then
							Link (Object, Output, Village_Id, Day + 1,
								User_Id => User_Id, User_Password => User_Password);
						else
							raise Program_Error;
						end if;
					end Handle_Next;
				begin
					Web.Producers.Produce(Output, Template, Handler => Handle_Next'Access);
				end;
			end if;
		elsif Tag = "scroll" then
			if Village.State /= Villages.Closed
				and then Day = Village.Today
				and then Player_Index >= 0
			then
				Web.Producers.Produce(Output, Template);
			end if;
		else
			Handle_Villages(Output, Tag, Template, Object,
				Village_Id, Village.all, Day, User_Id => User_Id, User_Password => User_Password);
		end if;
	end Handle;
begin
	Target_Day := Village.Today;
	if Village.Time = Villages.Night then
		Target_Day := Target_Day - 1;
	end if;
	Produce (Object, Output, Object.Configuration.Template_Village_File_Name.all, Handle'Access);
end Tabula.Renderers.Village_Page;
