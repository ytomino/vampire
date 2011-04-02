-- The Village of Vampire by YT, このソースコードはNYSLです
package body Vampire.Villages.Text is
	
	Line_Break : constant Character := ASCII.LF;
	
	-- 配役
	
	function Name (Person : Tabula.Villages.Person_Type'Class) return String is
	begin
		return Person.Work.Constant_Reference.Element.all &
			Person.Name.Constant_Reference.Element.all;
	end Name;
	
	function Image (Role : Requested_Role) return String is
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
	
	function Image (Role : Person_Role) return String is
	begin
		case Role is
			when Inhabitant | Loved_Inhabitant => return "善良な村人";
			when Unfortunate_Inhabitant => return "数奇な運命の村人";
			when Vampire_K | Vampire_Q | Vampire_J => return "吸血鬼";
			when Servant => return "吸血鬼の使徒";
			when Detective => return "探偵";
			when Astronomer => return "天文家";
			when Doctor => return "医者";
			when Hunter => return "猟師";
			when Lover | Sweetheart_M | Sweetheart_F => return "恋する村人";
			when Gremlin => return "妖魔";
		end case;
	end Image;
	
	function Short_Image (Role : Person_Role) return String is
	begin
		case Role is
			when Inhabitant | Loved_Inhabitant => return "村";
			when Unfortunate_Inhabitant => return "奇";
			when Vampire_K | Vampire_Q | Vampire_J => return "鬼";
			when Servant => return "使";
			when Detective => return "探";
			when Astronomer => return "天";
			when Doctor => return "医";
			when Hunter => return "猟";
			when Lover | Sweetheart_M | Sweetheart_F => return "恋";
			when Gremlin => return "妖";
		end case;
	end Short_Image;
	
	-- 参加
	
	function Join (Village : Village_Type; Message : Villages.Message)
		return String
	is
		Subject : Person_Type renames Village.People.Constant_Reference (Message.Subject).Element.all;
	begin
		return Image (Message.Subject + 1) & "人目に" & Name (Subject) & "が現れました。 ";
	end Join;
	
	function Escaped_Join (Village : Village_Type; Message : Villages.Message)
		return String
	is
		Subject : Person_Type renames Village.Escaped_People.Constant_Reference (Message.Subject).Element.all;
	begin
		return Name (Subject) & "が現れました。 ";
	end Escaped_Join;
	
	function Escape (Village : Village_Type; Message : Villages.Message)
		return String
	is
		Subject : Person_Type
			renames Village.Escaped_People.Constant_Reference (Message.Subject).Element.all;
	begin
		if Village.State >= Epilogue then
			return Name (Subject) & "(" &
				Subject.Id.Constant_Reference.Element.all &
				")は人知れず華やいだ都会へと旅立ってゆきました。 ";
		else
			return Name (Subject) & "は人知れず華やいだ都会へと旅立ってゆきました。 ";
		end if;
	end Escape;
	
	-- 舞台
	
	type Stage_Kind is (A_Village, A_Castle);
	type Stage_Type is record
		Introduction, Breakdown : not null access constant String;
	end record;
	
	Stages : constant array (Stage_Kind) of Stage_Type := (
		A_Village => (
			Introduction => new String'(
				"いつものように、血塗られた伝説が残る村を照らすお日さまが傾きかけました。 " &
				"慌ただしい人も暇な人も、子供もお年寄りも、一日の仕事を終えて、村人たちが酒場に集まる時間です。 " & Line_Break &
				"今夜は地主さんもやって来ますので、出迎えの準備もしなければなりません……。 "),
			Breakdown => new String'(
				"昨夜ついに来ることはなかった地主さんの死体が、翌朝村の真ん中にありました。 " & Line_Break &
				"死体は干乾び、首筋には牙のあとがありますが、これは獣のものではありません……。 " &
				"村人たちが不審がっていると、突如地主さんの死体が、赤い目を見開き牙を剥いて起き上がりました。 " &
				"しかし山間から差し込む朝日を浴びてその身体は灰となり崩れ落ちてゆきます……。 " & Line_Break &
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
	
	function Stage (Village : Village_Type) return Stage_Kind is
		L : constant Natural := Village.Name.Length;
	begin
		if L >= 3 and then Ada.Strings.Unbounded.Slice (Village.Name, L - 2, L) = "城" then
			return A_Castle;
		else
			return A_Village;
		end if;
	end Stage;
	
	function Introduction (Village : Village_Type) return String is
	begin
		return Stages (Stage (Village)).Introduction.all;
	end Introduction;
	
	function Breakdown (Village : Village_Type) return String is
	begin
		case Village.Execution is
			when Dummy_Killed_And_From_First | From_First =>
				return Stages (Stage (Village)).Breakdown.all & Line_Break & For_Execution_Message;
			when From_Second =>
				return Stages (Stage (Village)).Breakdown.all;
		end case;
	end Breakdown;
	
	function For_Execution_In_Second (Village : Village_Type) return String is
	begin
		case Village.Execution is
			when Dummy_Killed_And_From_First | From_First =>
				return "";
			when From_Second =>
				return For_Execution_Message;
		end case;
	end For_Execution_In_Second;
	
	-- 進行
	
	function Vampires (Village : Village_Type) return String is
		Result : aliased Ada.Strings.Unbounded.Unbounded_String;
	begin
		Ada.Strings.Unbounded.Append (Result, "その夜、草木も眠る頃、人知れず月を舞う影がありました……。 ");
		for I in Vampire_Role loop
			for Position in Village.People.First_Index .. Village.People.Last_Index loop
				declare
					P : Person_Type renames Village.People.Constant_Reference (Position).Element.all;
				begin
					if P.Role = I then
						if I /= Vampire_K then
							Ada.Strings.Unbounded.Append (Result, "、");
						end if;
						Ada.Strings.Unbounded.Append (Result, Name (P));
					end if;
				end;
			end loop;
		end loop;
		Ada.Strings.Unbounded.Append (Result, "。 村を見下ろすと、誰かが夜道を早足で歩いています……。 ");
		return Result.Constant_Reference.Element.all;
	end Vampires;
	
	function Teaming (Village : Village_Type) return String is
		Detective, Astronomer, Doctor, Hunter, Sweetheart, Lover, Unfortunate, Servant, Gremlin : Boolean := False;
		Vampire_Count : Natural := 0;
		Village_Side_Capabilityperson : Natural := 0;
		procedure Countup (Role : Person_Role) is
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
				when Sweetheart_M | Sweetheart_F =>
					Sweetheart := True;
					Village_Side_Capabilityperson := Village_Side_Capabilityperson + 1;
				when Unfortunate_Inhabitant =>
					Unfortunate := True;
					Village_Side_Capabilityperson := Village_Side_Capabilityperson + 1;
				when Villages.Servant =>
					Servant := True;
				when Vampire_Role =>
					Vampire_Count := Vampire_Count + 1;
				when Villages.Gremlin =>
					Gremlin := True;
				when Inhabitant | Loved_Inhabitant =>
					null;
			end case;
		end Countup;
		Result : aliased Ada.Strings.Unbounded.Unbounded_String;
	begin
		if Village.Execution = Dummy_Killed_And_From_First then
			Ada.Strings.Unbounded.Append (Result, "地主さんを含む" & Image (1 + Village.People.Length));
			Countup (Village.Dummy_Role);
		else
			Ada.Strings.Unbounded.Append (Result, Image (Village.People.Length));
		end if;
		Ada.Strings.Unbounded.Append (Result, "人の村人の中には");
		for Position in Village.People.First_Index .. Village.People.Last_Index loop
			Countup (Village.People.Constant_Reference (Position).Element.Role);
		end loop;
		if Village.Teaming in Hidings then
			Ada.Strings.Unbounded.Append (Result, Image (Village_Side_Capabilityperson));
			Ada.Strings.Unbounded.Append (Result, "人の能力者");
		else
			if Detective then
				Ada.Strings.Unbounded.Append (Result, "、探偵");
			end if;
			if Astronomer then
				Ada.Strings.Unbounded.Append (Result, "、天文家");
			end if;
			if Doctor then
				Ada.Strings.Unbounded.Append (Result, "、医者");
			end if;
			if Hunter then
				Ada.Strings.Unbounded.Append (Result, "、猟師");
			end if;
			if Lover then
				Ada.Strings.Unbounded.Append (Result, "、片想い");
			end if;
			if Sweetheart then
				Ada.Strings.Unbounded.Append (Result, "、恋人");
			end if;
			if Unfortunate then
				Ada.Strings.Unbounded.Append (Result, "、数奇な運命の村人");
			end if;
		end if;
		Ada.Strings.Unbounded.Append (Result, "がいます。 ");
		if Village.Monster_Side = Shuffling then
			Ada.Strings.Unbounded.Append (Result, "吸血鬼の全貌はわかりません……。 ");
		else
			Ada.Strings.Unbounded.Append (Result, "そして昨夜、月明かりに照らし出された人影が");
			Ada.Strings.Unbounded.Append (Result, Image (Vampire_Count));
			Ada.Strings.Unbounded.Append (Result, "つ……。 ");
			if Servant then
				case Village.Servant_Knowing is
					when None =>
						Ada.Strings.Unbounded.Append (Result, "それを崇める者……。 ");
					when Vampire_K =>
						Ada.Strings.Unbounded.Append (Result, "吸血鬼の王を目撃し魅了された者……。 ");
					when All_Vampires =>
						Ada.Strings.Unbounded.Append (Result, "吸血鬼の集いを目撃し魅了された者……。 ");
				end case;
			end if;
			if Gremlin then
				Ada.Strings.Unbounded.Append (Result, "さらに忍び寄る魔の手……。 ");
			end if;
		end if;
		return Result.Constant_Reference.Element.all;
	end Teaming;
	
	function Servant_Knew_Message (Village : Village_Type; Message : Villages.Message)
		return String
	is
		Subject : Person_Type renames Village.People.Constant_Reference (Message.Subject).Element.all;
		Result : aliased Ada.Strings.Unbounded.Unbounded_String;
	begin
		Ada.Strings.Unbounded.Append (Result, Name (Subject) & "は見てしまいました。 ");
		case Servant_Message_Kind (Message.Kind) is
			when Servant_Knew_Vampire_K =>
				Ada.Strings.Unbounded.Append (Result, "吸血鬼の王は");
				for Position in Village.People.First_Index .. Village.People.Last_Index loop
					declare
						P : Person_Type renames Village.People.Constant_Reference (Position).Element.all;
					begin
						if P.Role = Vampire_K then
							Ada.Strings.Unbounded.Append (Result, Name (P));
						end if;
					end;
				end loop;
				Ada.Strings.Unbounded.Append (Result, "です。 ");
			when Servant_Knew_Vampires =>
				Ada.Strings.Unbounded.Append (Result, "吸血鬼は");
				declare
					First : Boolean := True;
				begin
					for Role in Vampire_Role loop
						for Position in Village.People.First_Index .. Village.People.Last_Index loop
							declare
								P : Person_Type renames Village.People.Constant_Reference (Position).Element.all;
							begin
								if P.Role = Role then
									if not First then
										Ada.Strings.Unbounded.Append (Result, "、");
									end if;
									Ada.Strings.Unbounded.Append (Result, Name (P));
									First := False;
								end if;
							end;
						end loop;
					end loop;
				end;
				Ada.Strings.Unbounded.Append (Result, "です。 ");
		end case;
		return Result.Constant_Reference.Element.all;
	end Servant_Knew_Message;
	
	function Doctor_Cure_Message (Village : Village_Type; Message : Villages.Message)
		return String
	is
		Subject : Person_Type renames Village.People.Constant_Reference (Message.Subject).Element.all;
		Target : Person_Type renames Village.People.Constant_Reference (Message.Target).Element.all;
		Showing_Result : constant array (Doctor_Message_Kind) of not null access constant String := (
			Doctor_Found_Infection | Doctor_Found_Infection_Preview |
			Doctor_Cure | Doctor_Cure_Preview =>
				new String'("を診察し、首筋に牙の跡を見つけました。 " & Line_Break & "薬が効くことを祈りましょう。 "),
			Doctor_Failed | Doctor_Failed_Preview =>
				new String'("を診察しましたが、異常は見当たりませんでした。 "),
			Doctor_Found_Gremlin | Doctor_Found_Gremlin_Preview =>
				new String'("を診察しました。 " & Line_Break & "……妖魔だ！ "));
	begin
		return Name (Subject) & "は" & Name (Target) & Showing_Result (Message.Kind).all;
	end Doctor_Cure_Message;
	
	function Detective_Survey_Message (Village : Village_Type; Message : Villages.Message)
		return String
	is
		Subject : Person_Type renames Village.People.Constant_Reference (Message.Subject).Element.all;
		function Showing_Role (Role : Person_Role) return String is
		begin
			case Role is
				when Gremlin | Vampire_Role =>
					return "人間では無かった";
				when others =>
					return Image (Role) & "だった";
			end case;
		end Showing_Role;
		Role : Person_Role;
	begin
		case Detective_Message_Kind (Message.Kind) is
			when Detective_Survey | Detective_Survey_Preview =>
				declare
					Target : Person_Type renames Village.People.Constant_Reference (Message.Target).Element.all;
				begin
					if Village.Daytime_Preview = Message_Only and then Message.Kind = Detective_Survey_Preview then
						return Name (Subject) & "は" & Name (Target) & "を調査しました。 ";
					else
						Role := Target.Role;
						return Name (Subject) & "は" & Name (Target) & "を調査しました。 " & Line_Break &
							"どうやら" & Showing_Role (Role) & "ようです。 " & Line_Break;
					end if;
				end;
			when Detective_Survey_Victim =>
				return Name (Subject) & "は地主さんを調査しました。 " & Line_Break &
					"どうやら" & Showing_Role (Village.Dummy_Role) & "ようです。 " & Line_Break;
		end case;
	end Detective_Survey_Message;
	
	function Votes (
		Village : Village_Type;
		Day : Natural;
		Provisional : Boolean;
		Player_Index : Person_Index'Base)
		return String
	is
		Result : aliased Ada.Strings.Unbounded.Unbounded_String;
	begin
		for I in Village.People.First_Index .. Village.People.Last_Index loop
			declare
				P : Person_Type renames Village.People.Constant_Reference (I).Element.all;
				V : Integer;
			begin
				if Provisional then
					V := P.Records.Constant_Reference (Day).Element.Provisional_Vote;
				else
					V := P.Records.Constant_Reference (Day).Element.Vote;
				end if;
				if V in Village.People.First_Index .. Village.People.Last_Index
					and then (
						Village.State >= Epilogue
						or else Player_Index = I
						or else Village.Vote = Preliminary_And_Final)
				then
					declare
						T : Person_Type renames Village.People.Constant_Reference (V).Element.all;
					begin
						Ada.Strings.Unbounded.Append (
							Result,
							Name (P) & "は" & Name (T) & "に投票しました。 " & Line_Break);
					end;
				end if;
			end;
		end loop;
		return Result.Constant_Reference.Element.all;
	end Votes;
	
	function Votes_Totaled (
		Village : Village_Type;
		Day : Natural;
		Provisional : Boolean;
		Executed: Person_Index'Base)
		return String
	is
		Voted : constant Voted_Count_Info :=
			Village.Voted_Count (Day => Day, Provisional => Provisional);
		Result : aliased Ada.Strings.Unbounded.Unbounded_String;
	begin
		for Count in reverse 1 .. Voted.Max loop
			for Target_Index in Voted.Counts'Range loop
				if Voted.Counts (Target_Index) = Count then
					declare
						T : Person_Type renames Village.People.Constant_Reference (Target_Index).Element.all;
					begin
						Ada.Strings.Unbounded.Append (
							Result,
							Image (Voted.Counts (Target_Index)) & "票が" & Name (T) & "に集まりました。 " & Line_Break);
					end;
				end if;
			end loop;
		end loop;
		if Provisional then
			declare
				First : Boolean := True;
			begin
				Ada.Strings.Unbounded.Append (Result, "仮投票の結果、");
				for Count in reverse 1 .. Voted.Max loop
					for I in Village.People.First_Index .. Village.People.Last_Index loop
						if Voted.Counts (I) = Count then
							declare
								The_Person : Person_Type renames Village.People.Constant_Reference (I).Element.all;
							begin
								if The_Person.Records.Constant_Reference (Day).Element.Candidate
									and then The_Person.Records.Constant_Reference (Day).Element.State /= Died
								then
									if not First then
										Ada.Strings.Unbounded.Append (Result, "、");
									end if;
									Ada.Strings.Unbounded.Append (Result, Name (The_Person));
									First := False;
								end if;
							end;
						end if;
					end loop;
				end loop;
				Ada.Strings.Unbounded.Append (Result, "が本投票の候補になります。 ");
			end;
		else
			Ada.Strings.Unbounded.Append (
				Result,
				Name (Village.People.Constant_Reference (Executed).Element.all) & "は心臓に杭を打ち込まれました。 ");
		end if;
		return Result.Constant_Reference.Element.all;
	end Votes_Totaled;
	
	function Howling_Blocked (Village : Village_Type) return String is
		The_Unfortunate : constant Person_Index := Find_Superman (Village, Unfortunate_Inhabitant);
		P : Person_Type renames Village.People.Constant_Reference (The_Unfortunate).Element.all;
	begin
		return Name (P) & "のせいで用事ができてしまい、今夜は相談ができません。 ";
	end Howling_Blocked;
	
	function Astronomer_Observation_Message (Village : Village_Type; Message : Villages.Message)
		return String
	is
		Subject : Person_Type renames Village.People.Constant_Reference (Message.Subject).Element.all;
		Target : Person_Type renames Village.People.Constant_Reference (Message.Target).Element.all;
		function Showing_Result return String is
		begin
			if Target.Role in Vampire_Role
				or else Target.Role = Gremlin
				or else Target.Records.Constant_Reference (Message.Day - 1).Element.State = Infected
			then
				return "観測していて、人影が飛び立つのを目撃してしまいました。 ";
			else
				return "観測していました。 ";
			end if;
		end Showing_Result;
	begin
		return Name (Subject) & "は" & Name (Target) & "の家の上空を" & Showing_Result;
	end Astronomer_Observation_Message;
	
	function Hunter_Guard_Message (Village : Village_Type; Message : Villages.Message)
		return String
	is
		Subject : Person_Type renames Village.People.Constant_Reference (Message.Subject).Element.all;
	begin
		case Message.Kind is
			when Hunter_Nothing_With_Silver =>
				return Name (Subject) & "は銃に銀の弾丸を込めていましたが、その夜は何事もありませんでした。 ";
			when Hunter_Infected_With_Silver =>
				return Name (Subject) & "は銀の弾丸で吸血鬼を撃ち抜きました。 ";
			when Hunter_Killed_With_Silver =>
				return Name (Subject) & "は自らの命と引き換えに、銀の弾丸で吸血鬼を撃ち抜きました。 ";
			when others =>
				declare
					Target : Person_Type renames Village.People.Constant_Reference (Message.Target).Element.all;
				begin
					case Hunter_Message_Kind (Message.Kind) is
						when Hunter_Guard =>
							return Name (Subject) & "は" & Name (Target) & "を吸血鬼から守り抜きました。 ";
						when Hunter_Guard_With_Silver =>
							return Name (Subject) & "は" & Name (Target) & "を守り、銀の弾丸で吸血鬼を撃ち抜きました。 ";
						when Hunter_Nothing_With_Silver | Hunter_Infected_With_Silver | Hunter_Killed_With_Silver =>
							raise Program_Error;
						when Hunter_Failed =>
							return Name (Subject) & "は" & Name (Target) & "を守っていました。 ";
						when Hunter_Failed_With_Silver =>
							return Name (Subject) & "は" & Name (Target) & "を銀の弾丸で守っていました。 ";
					end case;
				end;
		end case;
	end Hunter_Guard_Message;
	
	function Vampire_Murder_Message (
		Village : Village_Type;
		Message : Villages.Message;
		Executed : Person_Index'Base)
		return String
	is
		Result : aliased Ada.Strings.Unbounded.Unbounded_String;
		Subject : Person_Type renames Village.People.Constant_Reference (Message.Subject).Element.all;
		Target : Person_Type renames Village.People.Constant_Reference (Message.Target).Element.all;
	begin
		if Subject.Role in Vampire_Role then
			for Role in Vampire_Role loop
				for I in Village.People.First_Index .. Village.People.Last_Index loop
					if I /= Executed then
						declare
							P : Person_Type renames Village.People.Constant_Reference (I).Element.all;
						begin
							if P.Role = Role then
								declare
									V : constant Integer := P.Records.Constant_Reference (Message.Day - 1).Element.Target;
								begin
									if V >= 0 then
										declare
											T : Person_Type renames Village.People.Constant_Reference (V).Element.all;
										begin
											Ada.Strings.Unbounded.Append (Result, Name (P) & "は" & Name (T) & "に目をつけました。 " & Line_Break);
										end;
									end if;
								end;
							end if;
						end;
					end if;
				end loop;
			end loop;
		else
			Ada.Strings.Unbounded.Append (Result, Name (Subject) & "は" & Name (Target) & "に目をつけました。 " & Line_Break);
		end if;
		Ada.Strings.Unbounded.Append (Result, "吸血鬼は" & Name (Target) & "を");
		case Vampire_Message_Kind (Message.Kind) is
			when Vampire_Murder =>
				Ada.Strings.Unbounded.Append (Result, "襲いました。 ");
			when Vampire_Murder_And_Killed =>
				Ada.Strings.Unbounded.Append (Result, "襲い、抵抗を受け殺されました。 ");
			when Vampire_Infection =>
				Ada.Strings.Unbounded.Append (Result, "感染させました。 ");
			when Vampire_Infection_And_Killed =>
				Ada.Strings.Unbounded.Append (Result, "感染させ、抵抗を受け殺されました。 ");
			when Vampire_Failed =>
				Ada.Strings.Unbounded.Append (Result, "襲おうとしましたが、何者かに妨げられました。 ");
			when Vampire_Failed_And_Killed =>
				Ada.Strings.Unbounded.Append (Result, "襲おうとしましたが、何者かに妨げられ殺されました。 ");
		end case;
		return Result.Constant_Reference.Element.all;
	end Vampire_Murder_Message;
	
	function Awareness (Village : Village_Type; Message : Villages.Message)
		return String
	is
		Subject : Person_Type renames Village.People.Constant_Reference (Message.Subject).Element.all;
	begin
		return Name (Subject) & "は無性に闇が恋しくなり……夜空へと飛び立ちました。 ";
	end Awareness;
	
	function Fatalities (Village : Village_Type; Day : Natural; Executed : Person_Index'Base)
		return String
	is
		pragma Assert (Day >= 2);
		Result : aliased Ada.Strings.Unbounded.Unbounded_String;
	begin
		for I in Village.People.First_Index .. Village.People.Last_Index loop
			declare
				P : Person_Type renames Village.People.Constant_Reference (I).Element.all;
			begin
				if P.Records.Constant_Reference (Day - 1).Element.State /= Died
					and then P.Records.Constant_Reference (Day).Element.State = Died
					and then Executed /= I
				then
					if not Result.Is_Null then
						Ada.Strings.Unbounded.Append (Result, "、");
					else
						Ada.Strings.Unbounded.Append (Result, "翌朝、");
					end if;
					Ada.Strings.Unbounded.Append (Result, Name (P));
				end if;
			end;
		end loop;
		if not Result.Is_Null then
			Ada.Strings.Unbounded.Append (Result, "の遺体が見つかりました……！ ");
		end if;
		return Result.Constant_Reference.Element.all;
	end Fatalities;
	
	function Survivors (Village : Village_Type; Day : Natural) return String is
		Result : aliased Ada.Strings.Unbounded.Unbounded_String;
	begin
		for I in Village.People.First_Index .. Village.People.Last_Index loop
			declare
				P : Person_Type renames Village.People.Constant_Reference (I).Element.all;
			begin
				if P.Records.Constant_Reference (Day).Element.State /= Died then
					if not Result.Is_Null then
						Ada.Strings.Unbounded.Append (Result, "、");
					end if;
					Ada.Strings.Unbounded.Append (Result, Name (P));
				end if;
			end;
		end loop;
		Ada.Strings.Unbounded.Append (Result, "が生存者です。 ");
		return Result.Constant_Reference.Element.all;
	end Survivors;
	
	function Gremlin_Sense (Village : Village_Type; Day : Natural) return String is
		Vampire_Count : Natural := 0;
	begin
		for I in Village.People.First_Index .. Village.People.Last_Index loop
			declare
				P : Person_Type renames Village.People.Constant_Reference (I).Element.all;
				R : Person_Record renames P.Records.Constant_Reference (Day).Element.all;
			begin
				if (R.State = Normal and then P.Role in Vampire_Role)
					or else R.State = Infected
				then
					Vampire_Count := Vampire_Count + 1;
				end if;
			end;
		end loop;
		return "残り吸血鬼の数は" & Image (Vampire_Count) & "匹……。 ";
	end Gremlin_Sense;
	
	function Sweetheart_Incongruity (Village : Village_Type; Message : Villages.Message)
		return String
	is
		S : Person_Type renames Village.People.Constant_Reference (Message.Subject).Element.all;
		T : Person_Type renames Village.People.Constant_Reference (Message.Target).Element.all;
	begin
		return Name (S) & "は" & Name (T) & "に違和感を感じました。 ";
	end Sweetheart_Incongruity;
	
	function Sweetheart_Suicide (Village : Village_Type; Message : Villages.Message)
		return String
	is
		S : Person_Type renames Village.People.Constant_Reference (Message.Subject).Element.all;
	begin
		return Name (S) & "は想い人の後を追いました。 ";
	end Sweetheart_Suicide;
	
	-- アクション
	
	function Action_Wake (Village : Village_Type; Message : Villages.Message)
		return String
	is
		Subject : Person_Type renames Village.People.Constant_Reference (Message.Subject).Element.all;
		Target : Person_Type renames Village.People.Constant_Reference (Message.Target).Element.all;
	begin
		return Name (Subject) & "は" & Name (Target) & "を起こした。 ";
	end Action_Wake;
	
	function Action_Encourage (Village : Village_Type; Message : Villages.Message)
		return String
	is
		Subject : Person_Type renames Village.People.Constant_Reference (Message.Subject).Element.all;
		Target : Person_Type renames Village.People.Constant_Reference (Message.Target).Element.all;
	begin
		return Name (Subject) & "は" & Name (Target) & "に話の続きを促した。 ";
	end Action_Encourage;
	
	function Action_Vampire_Gaze (Village : Village_Type; Message : Villages.Message)
		return String
	is
		Subject : Person_Type renames Village.People.Constant_Reference (Message.Subject).Element.all;
		Target : Person_Type renames Village.People.Constant_Reference (Message.Target).Element.all;
	begin
		return Name (Subject) & "は" & Villages.Text.Name (Target) & "をこっそりと見つめた。 ";
	end Action_Vampire_Gaze;
	
	function Action_Vampire_Gaze_Blocked (Village : Village_Type; Message : Villages.Message)
		return String
	is
		Subject : Person_Type renames Village.People.Constant_Reference (Message.Subject).Element.all;
		The_Unfortunate : constant Person_Index := Find_Superman (Village, Unfortunate_Inhabitant);
	begin
		return Name (Subject) & "の視線は" &
		Name (Village.People.Constant_Reference (The_Unfortunate).Element.all) &
		"に遮られた。 ";
	end Action_Vampire_Gaze_Blocked;
	
	-- 決着
	
	function People_In_Epilogure (Village : Village_Type) return String is
		Log : aliased Ada.Strings.Unbounded.Unbounded_String;
		Last_Day_Messages : Message_Counts renames Count_Messages (Village, Village.Today - 1);
		Second : Boolean := False;
	begin
		for I in Village.People.First_Index .. Village.People.Last_Index loop
			declare
				P : Person_Type renames Village.People.Constant_Reference (I).Element.all;
				Speech_Count : constant Natural := Last_Day_Messages (I).Speech;
			begin
				if Second then
					Ada.Strings.Unbounded.Append (Log, Line_Break);
				end if;
				Ada.Strings.Unbounded.Append (
					Log,
					Name (P) & "(" & P.Id.Constant_Reference.Element.all & ")は" &
					Image (P.Role) & "でした。 ");
				case P.Records.Constant_Reference (Village.Today).Element.State is
					when Normal =>
						Ada.Strings.Unbounded.Append (Log, "生存しました。 ");
						if Speech_Count = 0 then
							Ada.Strings.Unbounded.Append (Log, "蚊帳の外でした。 ");
						end if;
					when Infected =>
						Ada.Strings.Unbounded.Append (Log, "吸血鬼にされました。 ");
					when Died =>
						Ada.Strings.Unbounded.Append (Log, "死亡しました。 ");
				end case;
				Second := True;
			end;
		end loop;
		return Log.Constant_Reference.Element.all;
	end People_In_Epilogure;
	
	function Result_In_Epilogure (Village : Village_Type) return String is
		Last_Day_Messages : constant Message_Counts := Count_Messages (Village, Village.Today - 1);
		G_Win : Boolean := False;
		V_Win : Boolean := False;
	begin
		for I in Village.People.First_Index .. Village.People.Last_Index loop
			declare
				P : Person_Type renames Village.People.Constant_Reference (I).Element.all;
				Speech_Count : constant Natural := Last_Day_Messages (I).Speech;
			begin
				case P.Records.Constant_Reference (Village.Today).Element.State is
					when Normal =>
						case P.Role is
							when Vampire_Role =>
								if Speech_Count > 0 then
									V_Win := True;
								end if;
							when Gremlin =>
								if Speech_Count > 0 then
									G_Win := True;
								end if;
							when others => null;
						end case;
					when Infected | Died =>
						null;
				end case;
			end;
		end loop;
		if G_Win then
			if V_Win then
				return "村は吸血鬼の手に落ちた……流石の吸血鬼も安堵したその油断を突き……妖魔は全てに勝利しました。 ";
			else
				return "吸血鬼を退治した……その瞬間、彼奴を阻むものは何もなくなり……妖魔は全てに勝利しました。 ";
			end if;
		elsif V_Win then
			return "村は吸血鬼の手に落ちました。吸血鬼の勝利です！ ";
		else
			return "吸血鬼を退治しました。村人の勝利です！ ";
		end if;
	end Result_In_Epilogure;
	
end Vampire.Villages.Text;
