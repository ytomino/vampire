-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Calendar;
with Ada.Containers.Generic_Array_Sort;
with Ada.Numerics.Distributions;
with Ada.Strings.Unbounded;
with Vampire.Villages.Teaming;
procedure Vampire.Villages.Advance (
	Village : in out Village_Type;
	Now : in Ada.Calendar.Time;
	Generator : not null access Ada.Numerics.MT19937.Generator;
	Changed : out Boolean;
	List_Changed : out Boolean)
is
	use Person_Records;
	use Messages;
	use type Ada.Calendar.Time;
	use type Ada.Strings.Unbounded.Unbounded_String;
	
	subtype People_Index is Person_Index range Village.People.First_Index .. Village.People.Last_Index;
	function People_Random is
		new Ada.Numerics.Distributions.Linear_Discrete_Random (
			Ada.Numerics.MT19937.Cardinal,
			People_Index,
			Ada.Numerics.MT19937.Generator,
			Ada.Numerics.MT19937.Random_32);
	-- 日付変更
	procedure Increment_Today is
	begin
		Village.Today := Village.Today + 1;
		for Position in Village.People.First_Index .. Village.People.Last_Index loop
			declare
				P : Person_Type renames Village.People.Reference (Position).Element.all;
				New_Record : Person_Record := P.Records.Reference (P.Records.Last_Index).Element.all;
			begin
				New_Record.Vote := No_Person;
				New_Record.Provisional_Vote := No_Person;
				New_Record.Candidate := True;
				New_Record.Target := No_Person;
				New_Record.Special := False;
				if New_Record.State /= Died then
					New_Record.Note := Ada.Strings.Unbounded.Null_Unbounded_String;
				end if;
				Append (P.Records, New_Record);
				P.Commited := False;
			end;
		end loop;
	end Increment_Today;
	
	-- 一次開票
	procedure Preliminary_Vote (
		Village : in out Village_Type;
		Time : in Ada.Calendar.Time;
		Changed : in out Boolean)
	is
		type Voted_Array is array (Natural range <>) of Natural;
		procedure Sort is new Ada.Containers.Generic_Array_Sort (Natural, Natural, Voted_Array);
		Voted, Sort_Voted : Voted_Array (Village.People.First_Index .. Village.People.Last_Index) := (others => 0);
		Candidates : Natural := 0;
		Max : Natural := 0;
		Limit : Natural := 0;
	begin
		-- 集計
		for I in Village.People.First_Index .. Village.People.Last_Index loop
			if Village.People.Constant_Reference (I).Element.Records.Constant_Reference (Village.Today).Element.State /= Died then
				declare
					Target : Person_Index'Base := Village.People.Constant_Reference (I).Element.Records.Constant_Reference (Village.Today).Element.Provisional_Vote;
				begin
					if Target in Village.People.First_Index .. Village.People.Last_Index then
						if Voted (Target) = 0 then
							Candidates := Candidates + 1;
						end if;
						Voted (Target) := Voted (Target) + 1;
						if Voted (Target) > Max then
							Max := Voted (Target);
						end if;
					end if;
				end;
			end if;
		end loop;
		-- 候補が2名以上いる場合に適用
		if Candidates >= 2 then
			-- 同率2位までを候補とする
			Sort_Voted := Voted;
			Sort (Sort_Voted);
			Limit := Sort_Voted (Sort_Voted'Last - 1);
			for I in Village.People.First_Index .. Village.People.Last_Index loop
				declare
					The_Person : Person_Type renames Village.People.Reference (I).Element.all;
					The_Record : Person_Record renames The_Person.Records.Reference (Village.Today).Element.all;
				begin
					if The_Record.State /= Died then
						The_Record.Candidate := Voted (I) >= Limit;
					end if;
				end;
			end loop;
			-- 選ばれた候補以外に投票していた人は棄権に戻す
			for I in Village.People.First_Index .. Village.People.Last_Index loop
				declare
					V : Person_Index'Base renames Village.People.Reference (I).Element.Records.Reference (Village.Today).Element.Vote;
				begin
					if V >= 0 and then not Village.People.Constant_Reference (V).Element.Records.Constant_Reference (Village.Today).Element.Candidate then
						V := No_Person;
					end if;
				end;
			end loop;
			Append (
				Village.Messages,
				Message'(
					Kind => Preliminary_Vote,
					Day => Village.Today,
					Time => Time,
					Subject => No_Person,
					Target => No_Person,
					Text => Ada.Strings.Unbounded.Null_Unbounded_String));
			Changed := True; -- 変更を保存
		end if;
	end Preliminary_Vote;
	
	-- 処刑対象
	function Get_Execution (Execution_Day : Natural) return Person_Index'Base is
		Voted : array (People_Index) of Natural := (others => 0);
		Max : Natural := 0;
	begin
		for I in People_Index loop
			if Village.People.Constant_Reference (I).Element.Records.Constant_Reference (Execution_Day).Element.State /= Died then
				declare
					V : constant Person_Index'Base := Village.People.Constant_Reference (I).Element.Records.Constant_Reference (Execution_Day).Element.Vote;
				begin
					if V >= 0 then
						Voted (V) := Voted (V) + 1;
						if Voted (V) > Max then
							Max := Voted (V);
						end if;
					end if;
				end;
			end if;
		end loop;
		if Max > 0 then
			declare
				X : People_Index;
			begin
				loop
					X := People_Random (Generator);
					if Voted (X) = Max then
						return X;
					end if;
				end loop;
			end;
		end if;
		return No_Person;
	end Get_Execution;
	
	-- 全員会話(妨害を受ける)
	procedure Night_Talk_All (
		Village : in out Village_Type;
		Target_Day : in Natural;
		Time : in Ada.Calendar.Time) is
	begin
		for Rank in Vampire_Role loop
			declare
				Vampire : constant Person_Index'Base := Village.Find_Superman (Rank);
			begin
				if Vampire /= No_Person then
					declare
						Vampire_Person : Person_Type renames Village.People.Constant_Reference (Vampire).Element.all;
					begin
						if Vampire_Person.Records.Constant_Reference (Village.Today).Element.State /= Died
							and then not Vampire_Person.Records.Constant_Reference (Target_Day).Element.Note.Is_Null
						then
							Night_Talk (
								Village,
								Vampire,
								Vampire_Person.Records.Constant_Reference (Target_Day).Element.Note.Constant_Reference.Element.all,
								Time);
						end if;
					end;
				end if;
			end;
		end loop;
	end Night_Talk_All;
	
	-- 二次襲撃先の条件を満たしているか
	function Is_Secondary_Attackable (I : Person_Index) return Boolean is
	begin
		return Village.People.Constant_Reference (I).Element.Role not in Vampire_Role
			and then Village.People.Constant_Reference (I).Element.Role /= Gremlin
			and then Village.People.Constant_Reference (I).Element.Records.Constant_Reference (Village.Today).Element.State = Normal;
	end Is_Secondary_Attackable;
	
	-- 襲撃
	procedure Attack (
		Executed : in Person_Index'Base; -- 処刑された人
		Canceled : in Person_Index'Base; -- 二次襲撃キャンセル対象
		Infection_Only_In_First : in Boolean; -- 初日感染
		The_Hunter : in Person_Index'Base; -- 猟師
		The_Vampire : in Person_Index; -- 襲撃の主体
		Night_State : in Person_State; -- 主体の状態
		Attacked : out Boolean; -- 主体となって襲撃したかどうか(護衛されたかどうかは無関係)
		Guard : in Person_Index'Base; -- 猟師が護衛している人
		Silver_Bullet : in out Boolean; -- 銀の弾丸
		Guard_Succeed : in out Boolean) -- 護衛結果
	is
		Vampire_Person : Person_Type renames Village.People.Constant_Reference (The_Vampire).Element.all;
	begin
		Attacked := False;
		if Vampire_Person.Records.Constant_Reference (Village.Today).Element.State /= Died
			and then (Vampire_Person.Role in Vampire_Role or else Night_State = Infected)
		then
			declare
				Target : Person_Index'Base := Vampire_Person.Records.Constant_Reference (Village.Today - 1).Element.Target;
				Result : Vampire_Message_Kind := Vampire_Infection;
				Hunter_Result : Message_Kind;
			begin
				if Vampire_Person.Role not in Vampire_Role then
					-- 吸血鬼以外(感染夜間職)の襲撃先
					if Target = Executed or else Target = Canceled or else Target = No_Person then
						Target := No_Person;
						-- キャンセル対象または処刑対象を襲撃しようとした場合、他に振り替え
						-- 感染時点で使われた視線の中から未感染者を優先
						Find_Gaze : for I in reverse Village.Messages.First_Index .. Village.Messages.Last_Index loop
							declare
								Message : Villages.Message renames Village.Messages.Constant_Reference (I).Element.all;
							begin
								if Message.Kind = Action_Vampire_Gaze
									and then Vampire_Person.Records.Constant_Reference (Message.Day).Element.State = Infected -- 視線タイミングで感染していた？
									and then Message.Target /= Executed
									and then Message.Target /= Canceled
									and then Is_Secondary_Attackable (Message.Target)
								then
									Target := Message.Target;
									exit Find_Gaze;
								end if;
							end;
						end loop Find_Gaze;
						if Target = No_Person then
							-- 棄権はできない、ランダム(未感染者を優先)で襲う
							loop
								Target := People_Random (Generator);
								exit when Target /= The_Vampire and then Is_Secondary_Attackable (Target);
							end loop;
						end if;
					end if;
				end if;
				if Target /= No_Person
					and then Village.People.Constant_Reference (Target).Element.Records.Constant_Reference (Village.Today).Element.State /= Died
				then
					Attacked := True;
					if Vampire_Person.Role in Vampire_Role and then not Infection_Only_In_First then
						-- 吸血鬼の襲撃タイプ選択
						case Village.Attack is
							when Two | Nocturnal_Chain_Infecting =>
								for Rank2 in Person_Role'Succ (Vampire_Person.Role) .. Vampire_J loop
									declare
										Vampire2 : constant Person_Index'Base := Find_Superman (Village, Rank2);
									begin
										if Vampire2 >= 0
											and then Village.People.Constant_Reference (Vampire2).Element.Records.Constant_Reference (Village.Today).Element.State /= Died
											and then Target = Village.People.Constant_Reference (Vampire2).Element.Records.Constant_Reference (Village.Today - 1).Element.Target
										then
											Result := Vampire_Murder;
										end if;
									end;
								end loop;
							when Unanimity =>
								Result := Vampire_Murder;
								for Rank2 in Vampire_Role loop
									declare
										Vampire2 : constant Person_Index'Base := Find_Superman (Village, Rank2);
									begin
										if Vampire2 >= 0
											and then Village.People.Constant_Reference (Vampire2).Element.Records.Constant_Reference (Village.Today).Element.State /= Died
											and then Target /= Village.People.Constant_Reference (Vampire2).Element.Records.Constant_Reference (Village.Today - 1).Element.Target
										then
											Result := Vampire_Infection;
										end if;
									end;
								end loop;
						end case;
						-- 数奇な運命の村人は常に感染
						if Village.Unfortunate = Infected_Only
							and then Village.People.Constant_Reference (Target).Element.Role = Unfortunate_Inhabitant
						then
							Result := Vampire_Infection;
						end if;
					end if;
					if Guard /= Target then
						-- 襲撃成功、護衛失敗
						if Village.People.Constant_Reference (Target).Element.Role = Gremlin then
							-- 妖魔
							Result := Vampire_Failed;
						else
							if Result = Vampire_Murder then
								Village.People.Reference (Target).Element.Records.Reference (Village.Today).Element.State := Died;
								Hunter_Result := Hunter_Killed_With_Silver;
							else
								if Village.People.Constant_Reference (Target).Element.Role not in Vampire_Role then
									Village.People.Reference (Target).Element.Records.Reference (Village.Today).Element.State := Infected;
								end if;
								if Guard /= No_Person then
									Hunter_Result := Hunter_Guard_With_Silver;
								else
									Hunter_Result := Hunter_Infected_With_Silver;
								end if;
							end if;
							if Target = The_Hunter
								and then Silver_Bullet
								and then Village.Hunter_Silver_Bullet = Target_And_Self
							then
								-- 相打ち
								Guard_Succeed := True;
								Silver_Bullet := False;
								Village.People.Reference (The_Vampire).Element.Records.Reference (Village.Today).Element.State := Died;
								pragma Assert (not Infection_Only_In_First);
								Append (
									Village.Messages,
									Message'(
										Kind => Hunter_Result,
										Day => Village.Today,
										Time => Now,
										Subject => The_Hunter,
										Target => Guard,
										Text => Ada.Strings.Unbounded.Null_Unbounded_String));
								Result := Message_Kind'Succ (Result); -- _Killed
							end if;
						end if;
					else
						-- 護衛成功
						Guard_Succeed := True;
						if Silver_Bullet then
							Silver_Bullet := False;
							Village.People.Reference (The_Vampire).Element.Records.Reference (Village.Today).Element.State := Died;
							Hunter_Result := Hunter_Guard_With_Silver;
							Result := Vampire_Failed_And_Killed;
						else
							Hunter_Result := Hunter_Guard;
							Result := Vampire_Failed;
						end if;
						if not Infection_Only_In_First then
							Append (
								Village.Messages,
								Message'(
									Kind => Hunter_Result,
									Day => Village.Today,
									Time => Now,
									Subject => The_Hunter,
									Target => Target,
									Text => Ada.Strings.Unbounded.Null_Unbounded_String));
						end if;
					end if;
					-- 吸血鬼側のメッセージ
					if Infection_Only_In_First then
						case Result is
							when Vampire_Infection =>
								Result := Vampire_Infection_In_First;
							when Vampire_Failed =>
								Result := Vampire_Failed_In_First;
							when Vampire_Infection_In_First | Vampire_Failed_In_First
								| Vampire_Murder | Vampire_Murder_And_Killed
								| Vampire_Infection_And_Killed | Vampire_Failed_And_Killed =>
								raise Program_Error;
						end case;
					end if;
					Append (
						Village.Messages,
						Message'(
							Kind => Result,
							Day => Village.Today,
							Time => Now,
							Subject => The_Vampire,
							Target => Target,
							Text => Ada.Strings.Unbounded.Null_Unbounded_String));
				end if;
			end;
		end if;
	end Attack;
	
	Gremlin_Kill_Astronomer, Gremlin_Kill_Doctor : Boolean := False;
begin
	case Village.State is
		when Prologue =>
			Changed := False;
			List_Changed := False;
			-- 直接弄ってのリセット補助
			if Is_Empty (Village.Messages) then
				Append (
					Village.Messages,
					Message'(
						Kind => Introduction,
						Day => Village.Today,
						Time => Now,
						Subject => No_Person,
						Target => No_Person,
						Text => Ada.Strings.Unbounded.Null_Unbounded_String));
				Changed := True;
			end if;
			-- 強制キック
			declare
				Acted : Villages.Message_Counts renames Villages.Count_Messages(Village, 0);
			begin
				for I in reverse People_Index loop
					if Now - Acted(I).Last_Action_Time >= Escape_Duration(Village) then
						Escape(Village, I, Now);
						Changed := True;
						List_Changed := True;
					end if;
				end loop;
			end;
			-- 全員がコミットすると開始
			if Commit_Finished(Village) then
				-- 日付を更新
				Village.Dawn := Now - Village.Night_Duration;
				Increment_Today;
				Village.State := Playing;
				Village.Time := Daytime; -- 常に昼スタート
				-- 能力決定
				declare
					Sets : Teaming.Role_Set_Array renames Teaming.Possibilities (
						People_Count => Village.People.Length,
						Male_And_Female => Village.Male_And_Female,
						Execution => Village.Execution,
						Formation => Village.Formation,
						Unfortunate => Village.Unfortunate,
						Monster_Side => Village.Monster_Side);
					Set : Teaming.Role_Set renames Teaming.Select_Set (
						Sets => Sets,
						Appearance => Village.Appearance,
						Generator => Generator);
					Victim : access Person_Role := null;
				begin
					if Village.Execution = Dummy_Killed_And_From_First then
						Victim := Village.Dummy_Role'Access;
					end if;
					Teaming.Shuffle (
						People => Village.People,
						Victim => Victim,
						Set => Set,
						Generator => Generator);
				end;
				Append (
					Village.Messages,
					Message'(
						Kind => Breakdown,
						Day => Village.Today,
						Time => Now,
						Subject => No_Person,
						Target => No_Person,
						Text => Ada.Strings.Unbounded.Null_Unbounded_String));
				-- 初日犠牲者有りの時はひとりを観測済み
				if Village.Execution = Dummy_Killed_And_From_First then
					declare
						The_Astronomer : constant Person_Index'Base := Village.Find_Superman (Astronomer);
						Target : Person_Index'Base;
					begin
						if The_Astronomer >= 0 then
							loop
								Target := People_Random (Generator);
								case Village.People.Constant_Reference (Target).Element.Role is
									when Inhabitant | Loved_Inhabitant |
										Unfortunate_Inhabitant |
										Detective | Doctor | Astronomer | Hunter |
										Lover | Sweetheart_M | Sweetheart_F |
										Servant =>
										exit;
									when Vampire_Role | Gremlin =>
										null;
								end case;
							end loop;
							Append (
								Village.Messages,
								Message'(
									Kind => Astronomer_Observation,
									Day => Village.Today,
									Time => Now,
									Subject => The_Astronomer,
									Target => Target,
									Text => Ada.Strings.Unbounded.Null_Unbounded_String));
						end if;
					end;
				end if;
				-- 使徒が吸血鬼を知る
				if Village.Servant_Knowing /= None then
					declare
						The_Servant : constant Person_Index'Base := Find_Superman(Village, Servant);
					begin
						if The_Servant >= 0 then
							if Village.Servant_Knowing /= None then
								declare
									Kind : Message_Kind;
								begin
									case Village.Servant_Knowing is
										when None => raise Program_Error;
										when Vampire_K => Kind := Servant_Knew_Vampire_K;
										when All_Vampires => Kind := Servant_Knew_Vampires;
									end case;
									Append (
										Village.Messages,
										Message'(
											Kind => Kind,
											Day => Village.Today,
											Time => Now,
											Subject => The_Servant,
											Target => No_Person,
											Text => Ada.Strings.Unbounded.Null_Unbounded_String));
								end;
							end if;
						end if;
					end;
				end if;
				-- 探偵が初日犠牲者の役を知る
				if Village.Execution = Dummy_Killed_And_From_First then
					declare
						The_Detective : constant Person_Index'Base := Find_Superman(Village, Detective);
					begin
						if The_Detective >= 0 then
							Append (
								Village.Messages,
								Message'(
									Kind => Detective_Survey_Victim,
									Day => Village.Today,
									Time => Now,
									Subject => The_Detective,
									Target => No_Person,
									Text => Ada.Strings.Unbounded.Null_Unbounded_String));
						end if;
					end;
				end if;
				-- 更新通知
				Changed := True;
				List_Changed := True;
			end if;
		when Playing =>
			declare
				function Finished return Boolean is
					Inhabitant_Count, Vampire_Count : Natural := 0;
				begin
					for Position in People_Index loop
						if Village.People.Constant_Reference(Position).Element.Records.Constant_Reference(Village.Today).Element.State = Normal
							and not Village.People.Constant_Reference(Position).Element.Commited
						then
							case Village.People.Constant_Reference(Position).Element.Role is
								when Gremlin => null;
								when Vampire_Role => Vampire_Count := Vampire_Count + 1;
								when others => Inhabitant_Count := Inhabitant_Count + 1;
							end case;
						end if;
					end loop;
					return (Vampire_Count = 0) or (Inhabitant_Count <= Vampire_Count);
				end Finished;
				Daytime_To_Vote, Vote_To_Night, Night_To_Daytime : Boolean := False;
				Infection_In_First, Preliminary_Voting : Boolean := False;
				Executed : Person_Index'Base;
			begin
				case Village.Time is
					when Night =>
						if Now >= Village.Night_To_Daytime then
							Night_To_Daytime := True;
						end if;
					when Daytime =>
						-- 初日感染は飛ばさない
						if Village.Execution = Infection_And_From_First
							and then Village.Today = 1
							and then Now >= Village.Infection_In_First_Time
							and then not Village.Infected_In_First
						then
							Infection_In_First := True;
						end if;
						-- 更新
						if Now >= Village.Daytime_To_Vote
							or else Commit_Finished(Village)
						then
							Daytime_To_Vote := True;
							if Village.Day_Duration >= 24 * 60 * 60.0
								or else Vote_Finished (Village)
								or else Village.Vote_State = Disallowed
							then
								Vote_To_Night := True;
								if Village.Night_Duration = 0.0 then
									Night_To_Daytime := True;
								end if;
							end if;
						elsif Village.Vote = Preliminary_And_Final
							and then Now >= Village.Preliminary_Vote_Time
							and then Village.Vote_State = Allowed_For_Preliminary
						then
							Preliminary_Voting := True; -- コミットで飛ぶ
						end if;
					when Vote =>
						if Now >= Village.Vote_To_Night
							or else Vote_Finished(Village)
						then
							Vote_To_Night := True;
							if Village.Night_Duration = 0.0 then
								Night_To_Daytime := True;
							end if;
						end if;
				end case;
				Changed := Daytime_To_Vote or Vote_To_Night or Night_To_Daytime;
				List_Changed := Vote_To_Night;
				-- 初日感染
				if Infection_In_First then
					Night_Talk_All (Village, Village.Today, Now); -- 会話
					declare
						The_Hunter : constant Person_Index'Base := Village.Find_Superman (Hunter);
						Guard : Person_Index'Base := No_Person;
						Silver_Bullet : Boolean := False;
						Guard_Succeed : Boolean := False;
						Attacked : Boolean;
					begin
						if The_Hunter /= No_Person then
							pragma Assert (Village.People.Constant_Reference (The_Hunter).Element.Records.Constant_Reference (Village.Today).Element.State = Normal);
							Guard := Village.People.Constant_Reference (The_Hunter).Element.Records.Constant_Reference (Village.Today - 1).Element.Target;
						end if;
						for Rank in Vampire_Role loop
							declare
								The_Vampire : constant Person_Index'Base := Village.Find_Superman (Rank);
							begin
								if The_Vampire /= No_Person then
									Attack (
										Executed => No_Person,
										Canceled => No_Person,
										Infection_Only_In_First => True,
										The_Vampire => The_Vampire,
										Night_State => Infected,
										Attacked => Attacked,
										The_Hunter => The_Hunter,
										Guard => Guard,
										Silver_Bullet => Silver_Bullet,
										Guard_Succeed => Guard_Succeed);
									exit when Attacked;
								end if;
							end;
						end loop;
						-- 護衛記録だけ
						if Guard /= No_Person then
							Append (
								Village.Messages,
								Message'(
									Kind => Hunter_Guard_No_Response,
									Day => Village.Today,
									Time => Now,
									Subject => The_Hunter,
									Target => Guard,
									Text => Ada.Strings.Unbounded.Null_Unbounded_String));
						end if;
						-- 初日感染時間が過ぎたことを記録
						Append (
							Village.Messages,
							Message'(
								Kind => Foreboding,
								Day => Village.Today,
								Time => Now,
								Subject => No_Person,
								Target => No_Person,
								Text => Ada.Strings.Unbounded.Null_Unbounded_String));
					end;
					Changed := True;
				end if;
				-- 一次開票
				if Preliminary_Voting then
					Preliminary_Vote (Village, Now, Changed);
				end if;
				-- 昼から投票待ちへ
				if Daytime_To_Vote then
					Village.Time := Vote;
				end if;
				-- 投票待ちから夜へ
				if Vote_To_Night then
					Village.Time := Night;
					-- 日付を更新
					Village.Dawn := Now;
					Increment_Today;
					-- 医者 (生存しているかぎり、感染すると治療はなし)
					declare
						The_Doctor : constant Person_Index'Base := Find_Superman(Village, Doctor);
					begin
						if The_Doctor >= 0 and then Village.People.Constant_Reference(The_Doctor).Element.Records.Constant_Reference(Village.Today).Element.State /= Died then
							declare
								Target : constant Person_Index'Base := Village.People.Constant_Reference(The_Doctor).Element.Records.Constant_Reference(Village.Today - 1).Element.Target;
								Kind : Message_Kind;
							begin
								if Target >= 0 and then Village.People.Constant_Reference(Target).Element.Records.Constant_Reference(Village.Today).Element.State /= Died then
									if Village.People.Constant_Reference(Target).Element.Role = Gremlin then
										Gremlin_Kill_Doctor := True;
										Kind := Doctor_Found_Gremlin;
									elsif Village.People.Constant_Reference(Target).Element.Records.Constant_Reference(Village.Today).Element.State = Infected then
										if Village.Doctor_Infected = Find_Infection
											and then Village.People.Constant_Reference(The_Doctor).Element.Records.Constant_Reference(Village.Today).Element.State = Infected
										then
											Kind := Doctor_Found_Infection;
										else
											-- 治療成功
											Village.People.Reference(Target).Element.Records.Reference(Village.Today).Element.State := Normal;
											Kind := Doctor_Cure;
										end if;
									else
										Kind := Doctor_Failed;
									end if;
									Append(Village.Messages, (
										Kind => Kind,
										Day => Village.Today,
										Time => Now,
										Subject => The_Doctor,
										Target => Target,
										Text => Ada.Strings.Unbounded.Null_Unbounded_String));
								end if;
							end;
						end if;
					end;
					-- 探偵 (生存しているかぎり)
					declare
						The_Detective : constant Person_Index'Base := Find_Superman(Village, Detective);
					begin
						if The_Detective >= 0 and then Village.People.Constant_Reference(The_Detective).Element.Records.Constant_Reference(Village.Today).Element.State /= Died then
							if Village.Today <= 2 then
								if Village.Execution = Dummy_Killed_And_From_First then
									Append(Village.Messages, (
										Kind => Detective_Survey_Victim,
										Day => Village.Today,
										Time => Now,
										Subject => The_Detective,
										Target => No_Person,
										Text => Ada.Strings.Unbounded.Null_Unbounded_String));
								end if;
							else
								declare
									Target : constant Person_Index'Base := Village.People.Constant_Reference(The_Detective).Element.Records.Constant_Reference(Village.Today - 1).Element.Target;
								begin
									if Target >= 0 and then Village.People.Constant_Reference(Target).Element.Records.Constant_Reference(Village.Today - 1).Element.State = Died then
										Append(Village.Messages, (
											Kind => Detective_Survey,
											Day => Village.Today,
											Time => Now,
											Subject => The_Detective,
											Target => Target,
											Text => Village.People.Constant_Reference(Target).Element.Records.Constant_Reference(Village.Today - 1).Element.Note));
									end if;
								end;
							end if;
						end if;
					end;
					-- 処刑
					Executed := Get_Execution(Village.Today - 1);
					if Executed >= 0 then
						Village.People.Reference(Executed).Element.Records.Reference(Village.Today).Element.State := Died;
						Append(Village.Messages, (
							Kind => Execution,
							Day => Village.Today,
							Time => Now,
							Subject => No_Person,
							Target => Executed,
							Text => Ada.Strings.Unbounded.Null_Unbounded_String));
					end if;
					-- 吸血鬼の会話があらかじめセットされていたら転写
					if not Night_To_Daytime then
						Night_Talk_All (Village, Village.Today - 1, Now);
					end if;
				end if;
				-- 夜から昼へ
				if Night_To_Daytime then
					Village.Time := Daytime;
					-- 誰が処刑されたかを拾っておく
					Executed := No_Person;
					Find_Executed : for I in reverse Village.Messages.First_Index .. Village.Messages.Last_Index loop
						declare
							Message : Villages.Message renames Element(Village.Messages, I);
						begin
							if Message.Day = Village.Today and then Message.Kind = Execution then
								Executed := Message.Target;
								exit;
							elsif Message.Day < Village.Today then
								exit;
							end if;
						end;
					end loop Find_Executed;
					-- 自覚症状 (猟師と天文家のみ)
					for I in People_Index loop
						if Village.People.Constant_Reference(I).Element.Records.Constant_Reference(Village.Today).Element.State = Infected then
							case Village.People.Constant_Reference(I).Element.Role is
								when Hunter | Astronomer =>
									Append(Village.Messages, (
										Kind => Awareness,
										Day => Village.Today,
										Time => Now,
										Subject => I,
										Target => No_Person,
										Text => Ada.Strings.Unbounded.Null_Unbounded_String));
								when others => null;
							end case;
						end if;
					end loop;
					-- 観測 (感染すると無効)
					declare
						The_Astronomer : constant Person_Index'Base := Find_Superman(Village, Astronomer);
					begin
						if The_Astronomer >= 0 and then Village.People.Constant_Reference(The_Astronomer).Element.Records.Constant_Reference(Village.Today).Element.State = Normal then
							declare
								Target : constant Person_Index'Base := Village.People.Constant_Reference(The_Astronomer).Element.Records.Constant_Reference(Village.Today - 1).Element.Target;
							begin
								if Target >= 0 and then Village.People.Constant_Reference(Target).Element.Records.Constant_Reference(Village.Today).Element.State /= Died then
									Append(Village.Messages, (
										Kind => Astronomer_Observation,
										Day => Village.Today,
										Time => Now,
										Subject => The_Astronomer,
										Target => Target,
										Text => Ada.Strings.Unbounded.Null_Unbounded_String));
									if Village.People.Constant_Reference(Target).Element.Role = Gremlin then
										Gremlin_Kill_Astronomer := True;
									end if;
								end if;
							end;
						end if;
					end;
					-- 吸血鬼の会話 (夜→昼の1発言は数奇な運命の村人に妨害されない)
					Vampire_Meeting : for Rank in Vampire_Role loop
						declare
							Vampire : constant Person_Index'Base := Find_Superman(Village, Rank);
						begin
							if Vampire >= 0 and then Village.People.Constant_Reference(Vampire).Element.Records.Constant_Reference(Village.Today).Element.State /= Died then
								if Village.People.Constant_Reference(Vampire).Element.Records.Constant_Reference(Village.Today - 1).Element.Note /= Ada.Strings.Unbounded.Null_Unbounded_String then
									Append(Village.Messages, (
										Kind => Meeting,
										Day => Village.Today,
										Time => Now,
										Subject => No_Person,
										Target => No_Person,
										Text => Ada.Strings.Unbounded.Null_Unbounded_String));
									exit Vampire_Meeting;
								end if;
							end if;
						end;
					end loop Vampire_Meeting;
					-- 襲撃 (感染すると天文家、猟師または医者も)、と、ガード (感染すると無効)
					declare
						The_Hunter : constant Person_Index'Base := Village.Find_Superman (Hunter);
						The_Astronomer : constant Person_Index'Base := Village.Find_Superman (Astronomer);
						Guard : Person_Index'Base := No_Person;
						Silver_Bullet : Boolean := False;
						Guard_Succeed : Boolean := False;
						Attacked : Boolean;
						Astronomer_State, Hunter_State : Person_State; -- 連鎖が起きないように状態を保存
						Canceled : Person_Index'Base := No_Person; -- 襲撃取りやめアクションの対象
					begin
						-- キャンセル先
						Find_Cancel : for I in reverse Village.Messages.First_Index .. Village.Messages.Last_Index loop
							declare
								Message : Villages.Message renames Village.Messages.Constant_Reference (I).Element.all;
							begin
								if Message.Kind = Action_Vampire_Cancel then
									Canceled := Message.Target;
									exit Find_Cancel;
								elsif Message.Day < Village.Today - 1 then
									exit Find_Cancel;
								end if;
							end;
						end loop Find_Cancel;
						-- 猟師の行動確認
						if The_Hunter /= No_Person then
							Hunter_State := Village.People.Constant_Reference (The_Hunter).Element.Records.Constant_Reference (Village.Today).Element.State;
							if Hunter_State = Normal then
								Guard := Village.People.Constant_Reference(The_Hunter).Element.Records.Constant_Reference(Village.Today - 1).Element.Target;
								Silver_Bullet := Village.People.Constant_Reference(The_Hunter).Element.Records.Constant_Reference(Village.Today - 1).Element.Special;
							end if;
						end if;
						if The_Astronomer /= No_Person then
							Astronomer_State := Village.People.Constant_Reference(The_Astronomer).Element.Records.Constant_Reference(Village.Today).Element.State;
						end if;
						-- 吸血鬼の襲撃
						for Rank in Vampire_Role loop
							declare
								The_Vampire : constant Person_Index'Base := Village.Find_Superman (Rank);
							begin
								if The_Vampire /= No_Person then
									Attack (
										Executed => Executed,
										Canceled => No_Person,
										Infection_Only_In_First => False,
										The_Vampire => The_Vampire,
										Night_State => Infected,
										Attacked => Attacked,
										The_Hunter => The_Hunter,
										Guard => Guard,
										Silver_Bullet => Silver_Bullet,
										Guard_Succeed => Guard_Succeed);
									exit when Attacked;
								end if;
							end;
						end loop;
						case Village.Attack is
							when Nocturnal_Chain_Infecting =>
								-- 天文家の襲撃
								if The_Astronomer /= No_Person then
									Attack (
										Executed => Executed,
										Canceled => Canceled,
										Infection_Only_In_First => False,
										The_Vampire => The_Astronomer,
										Night_State => Astronomer_State,
										Attacked => Attacked,
										The_Hunter => The_Hunter,
										Guard => Guard,
										Silver_Bullet => Silver_Bullet,
										Guard_Succeed => Guard_Succeed);
								end if;
								-- 猟師の襲撃
								if The_Hunter /= No_Person then
									Attack (
										Executed => Executed,
										Canceled => Canceled,
										Infection_Only_In_First => False,
										The_Vampire => The_Hunter,
										Night_State => Hunter_State,
										Attacked => Attacked,
										The_Hunter => The_Hunter,
										Guard => Guard,
										Silver_Bullet => Silver_Bullet,
										Guard_Succeed => Guard_Succeed);
								end if;
							when others =>
								null;
						end case;
						-- 襲撃が来なかった時の猟師
						if not Guard_Succeed then
							if Guard >= 0 then
								declare
									Hunter_Result : Message_Kind;
								begin
									if Silver_Bullet then
										Hunter_Result := Hunter_Failed_With_Silver;
									else
										Hunter_Result := Hunter_Failed;
									end if;
									Append(Village.Messages, (
										Kind => Hunter_Result,
										Day => Village.Today,
										Time => Now,
										Subject => The_Hunter,
										Target => Guard,
										Text => Ada.Strings.Unbounded.Null_Unbounded_String));
								end;
							elsif Silver_Bullet then
								Append(Village.Messages, (
									Kind => Hunter_Nothing_With_Silver,
									Day => Village.Today,
									Time => Now,
									Subject => The_Hunter,
									Target => No_Person,
									Text => Ada.Strings.Unbounded.Null_Unbounded_String));
							end if;
						end if;
					end;
					-- 妖魔
					declare
						The_Gremlin : constant Person_Index'Base := Find_Superman(Village, Gremlin);
					begin
						if The_Gremlin >= 0 and then Village.People.Constant_Reference(The_Gremlin).Element.Records.Constant_Reference(Village.Today).Element.State /= Died then
							Append(Village.Messages, (
								Kind => Gremlin_Sense,
								Day => Village.Today,
								Time => Now,
								Subject => The_Gremlin,
								Target => No_Person,
								Text => Ada.Strings.Unbounded.Null_Unbounded_String));
						end if;
					end;
					if Gremlin_Kill_Doctor then
						declare
							The_Doctor : constant Person_Index'Base := Find_Superman(Village, Doctor);
						begin
							pragma Assert(The_Doctor >= 0);
							Village.People.Reference(The_Doctor).Element.Records.Reference(Village.Today).Element.State := Died;
						end;
					end if;
					if Gremlin_Kill_Astronomer then
						declare
							The_Astronomer : constant Person_Index'Base := Find_Superman(Village, Astronomer);
						begin
							pragma Assert(The_Astronomer >= 0);
							Village.People.Reference(The_Astronomer).Element.Records.Reference(Village.Today).Element.State := Died;
						end;
					end if;
					-- 恋人
					declare
						procedure Love_Process(Lover_Role, Loved_Role : Person_Role) is
							Lover_Person : constant Person_Index'Base := Find_Superman(Village, Lover_Role);
							Loved_Person : constant Person_Index'Base := Find_Superman(Village, Loved_Role);
						begin
							if Lover_Person >= 0 and then Loved_Person >= 0 then
								if Village.People.Constant_Reference(Lover_Person).Element.Records.Constant_Reference(Village.Today).Element.State /= Died then
									case Village.People.Constant_Reference(Loved_Person).Element.Records.Constant_Reference(Village.Today).Element.State is
										when Died =>
											Village.People.Reference(Lover_Person).Element.Records.Reference(Village.Today).Element.State := Died;
											Append(Village.Messages, (
												Kind => Sweetheart_Suicide,
												Day => Village.Today,
												Time => Now,
												Subject => Lover_Person,
												Target => Loved_Person,
												Text => Ada.Strings.Unbounded.Null_Unbounded_String));
										when Infected =>
											Append(Village.Messages, (
												Kind => Sweetheart_Incongruity,
												Day => Village.Today,
												Time => Now,
												Subject => Lover_Person,
												Target => Loved_Person,
												Text => Ada.Strings.Unbounded.Null_Unbounded_String));
										when Normal =>
											null;
									end case;
								end if;
							end if;
						end;
					begin
						Love_Process(Lover, Loved_Inhabitant);
						Love_Process(Sweetheart_M, Sweetheart_F);
						Love_Process(Sweetheart_F, Sweetheart_M);
					end;
					-- 生存者リスト
					Append(Village.Messages, (
						Kind => List,
						Day => Village.Today,
						Time => Now,
						Subject => No_Person,
						Target => No_Person,
						Text => Ada.Strings.Unbounded.Null_Unbounded_String));
					-- 生きていて前日発言が無い場合は強制コミット
					declare
						Said : Message_Counts renames Count_Messages(Village, Village.Today - 1);
					begin
						for Position in People_Index loop
							if Village.People.Constant_Reference(Position).Element.Records.Constant_Reference(Village.Today).Element.State /= Died
								and then Said(Position).Speech = 0
							then
								Village.People.Reference(Position).Element.Commited := True;
							end if;
						end loop;
					end;
					-- ラストの視線が襲撃デフォルト
					declare
						Target : Person_Index'Base := No_Person;
					begin
						Find_Gaze : for I in reverse 0 .. Village.Messages.Last_Index loop
							if Element(Village.Messages, I).Kind = Action_Vampire_Gaze then
								Target := Element(Village.Messages, I).Target;
								exit Find_Gaze;
							end if;
						end loop Find_Gaze;
						if Target >= 0 and then Village.People.Constant_Reference(Target).Element.Records.Constant_Reference(Village.Today).Element.State /= Died then
							for Position in People_Index loop
								if Village.People.Constant_Reference(Position).Element.Records.Constant_Reference(Village.Today).Element.State /= Died
									and then Village.People.Constant_Reference(Position).Element.Role in Vampire_Role
								then
									Village.People.Reference(Position).Element.Records.Reference(Village.Today).Element.Target := Target;
								end if;
							end loop;
						end if;
					end;
					-- 終了条件チェック
					if Finished then
						Village.State := Epilogue;
						-- 全員コミット解除
						for I in People_Index loop
							Village.People.Reference(I).Element.Commited := False;
						end loop;
					end if;
				end if;
			end;
		when Epilogue =>
			Changed := Now - Village.Dawn >= Duration'Max (
				Village.Day_Duration,
				Epilogue_Min_Duration); --  エピローグは最低でも1時間
			List_Changed := Changed;
			if Changed then
				Village.Dawn := Now;
				Village.State := Closed;
			end if;
		when Closed =>
			Changed := False;
			List_Changed := False;
	end case;
end Vampire.Villages.Advance;
