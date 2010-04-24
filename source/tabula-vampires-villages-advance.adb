-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Calendar;
with Ada.Strings.Unbounded;
with Tabula.Vampires.Villages.Teaming;
procedure Tabula.Vampires.Villages.Advance(
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
	
	function "+" (S : Ada.Strings.Unbounded.Unbounded_String) return String renames Ada.Strings.Unbounded.To_String;
	
	subtype People_Index is Natural range Village.People.First_Index .. Village.People.Last_Index;
	package People_Random is new Ada.Numerics.MT19937.Discrete_Random(People_Index);
	procedure Increment_Today is
	begin
		Village.Today := Village.Today + 1;
		for Position in Village.People.First_Index .. Village.People.Last_Index loop
			declare
				P : Person_Type renames Village.People.Reference(Position).Element.all;
				New_Record : Person_Record := P.Records.Reference(P.Records.Last_Index).Element.all;
			begin
				New_Record.Vote := -1;
				New_Record.Provisional_Vote := -1;
				New_Record.Candidate := True;
				New_Record.Target := -1;
				New_Record.Special := False;
				if New_Record.State /= Died then
					New_Record.Note := Ada.Strings.Unbounded.Null_Unbounded_String;
				end if;
				Append(P.Records, New_Record);
				P.Commited := False;
			end;
		end loop;
	end Increment_Today;
	function Get_Execution(Execution_Day : Natural) return Integer is
		Voted : array (People_Index) of Integer := (others => 0);
		Max : Integer := 0;
	begin
		for I in People_Index loop
			if Village.People.Constant_Reference(I).Element.Records.Constant_Reference(Execution_Day).Element.State /= Died then
				declare
					V : constant Integer := Village.People.Constant_Reference(I).Element.Records.Constant_Reference(Execution_Day).Element.Vote;
				begin
					if V >= 0 then
						Voted(V) := Voted(V) + 1;
						if Voted(V) > Max then
							Max := Voted(V);
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
					X := People_Random.Random(Generator);
					if Voted(X) = Max then
						return X;
					end if;
				end loop;
			end;
		end if;
		return -1;
	end Get_Execution;
	Gremlin_Kill_Astronomer, Gremlin_Kill_Doctor : Boolean := False;
begin
	case Village.State is
		when Tabula.Villages.Prologue =>
			Changed := False;
			List_Changed := False;
			-- 直接弄ってのリセット補助
			if Is_Empty(Village.Messages) then
				Append(Village.Messages, Message'(
					Kind => Introduction,
					Day => Village.Today,
					Time => Now,
					Subject => -1,
					Target => -1,
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
				Village.State := Tabula.Villages.Opened;
				Village.Time := Tabula.Villages.Daytime; -- 常に昼スタート
				-- 能力決定
				declare
					Sets : Teaming.Role_Set_Array renames Teaming.Possibilities (
						People_Count => Village.People.Length,
						Male_And_Female => Male_And_Female (Village.People),
						Execution => Village.Execution,
						Teaming => Village.Teaming,
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
				Append (Village.Messages, (
					Kind => Breakdown,
					Day => Village.Today,
					Time => Now,
					Subject => -1,
					Target => -1,
					Text => Ada.Strings.Unbounded.Null_Unbounded_String));
				-- 初日犠牲者有りの時はひとりを観測済み
				if Village.Execution = Dummy_Killed_And_From_First then
					declare
						The_Astronomer : constant Integer := Village.Find_Superman (Astronomer);
						Target : Integer;
					begin
						if The_Astronomer >= 0 then
							loop
								Target := People_Random.Random (Generator);
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
							Append (Village.Messages, (
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
						The_Servant : constant Integer := Find_Superman(Village, Servant);
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
									Append(Village.Messages, (
										Kind => Kind,
										Day => Village.Today,
										Time => Now,
										Subject => The_Servant,
										Target => -1,
										Text => Ada.Strings.Unbounded.Null_Unbounded_String));
								end;
							end if;
						end if;
					end;
				end if;
				-- 探偵が初日犠牲者の役を知る
				if Village.Execution = Dummy_Killed_And_From_First then
					declare
						The_Detective : constant Integer := Find_Superman(Village, Detective);
					begin
						if The_Detective >= 0 then
							Append(Village.Messages, (
								Kind => Detective_Survey_Victim, 
								Day => Village.Today,
								Time => Now,
								Subject => The_Detective,
								Target => -1,
								Text => Ada.Strings.Unbounded.Null_Unbounded_String));
						end if;
					end;
				end if;
				-- 更新通知
				Changed := True;
				List_Changed := True;
			end if;
		when Tabula.Villages.Opened =>
			declare
				function Finished return Boolean is
					Inhabitant_Count, Vampire_Count : Integer := 0;
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
				Provisional_Voting : Boolean := False;
				Executed : Integer;
			begin
				case Village.Time is
					when Tabula.Villages.Night =>
						if Now >= Village.Dawn + Village.Night_Duration then
							Night_To_Daytime := True;
						end if;
					when Tabula.Villages.Daytime =>
						if Now >= Village.Dawn + (Village.Day_Duration + Village.Night_Duration) 
							or else Commit_Finished(Village)
						then
							Daytime_To_Vote := True;
							if Village.Day_Duration >= 24 * 60 * 60.0
								or else Vote_Finished (Village)
								or else not Village.Be_Voting
							then
								Vote_To_Night := True;
								if Village.Night_Duration = 0.0 then
									Night_To_Daytime := True;
								end if;
							end if;
						elsif Now >= Village.Dawn + Village.Day_Duration / 2
							and then Village.Execution = Provisional_Voting_From_Second
							and then not Village.Provisional_Voted
						then
							Provisional_Voting := True;
						end if;
					when Tabula.Villages.Vote =>
						if Now >= Village.Dawn + (Village.Day_Duration + Village.Night_Duration + Vote_Duration) 
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
				-- 仮投票
				if Provisional_Voting then
					Provisional_Vote (Village, Now, Changed);
				end if;
				-- 昼から投票待ちへ
				if Daytime_To_Vote then
					Village.Time := Tabula.Villages.Vote;
				end if;
				-- 投票待ちから夜へ
				if Vote_To_Night then
					Village.Time := Tabula.Villages.Night;
					-- 日付を更新
					Village.Dawn := Now;
					Increment_Today;
					-- 医者 (生存しているかぎり、感染すると治療はなし)
					declare
						The_Doctor : constant Integer := Find_Superman(Village, Doctor);
					begin
						if The_Doctor >= 0 and then Village.People.Constant_Reference(The_Doctor).Element.Records.Constant_Reference(Village.Today).Element.State /= Died then
							declare
								Target : constant Integer := Village.People.Constant_Reference(The_Doctor).Element.Records.Constant_Reference(Village.Today - 1).Element.Target;
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
						The_Detective : constant Integer := Find_Superman(Village, Detective);
					begin
						if The_Detective >= 0 and then Village.People.Constant_Reference(The_Detective).Element.Records.Constant_Reference(Village.Today).Element.State /= Died then
							if Village.Today <= 2 then
								if Village.Execution = Dummy_Killed_And_From_First then
									Append(Village.Messages, (
										Kind => Detective_Survey_Victim,
										Day => Village.Today,
										Time => Now,
										Subject => The_Detective,
										Target => -1,
										Text => Ada.Strings.Unbounded.Null_Unbounded_String));
								end if;
							else
								declare
									Target : constant Integer := Village.People.Constant_Reference(The_Detective).Element.Records.Constant_Reference(Village.Today - 1).Element.Target;
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
							Subject => -1,
							Target => Executed,
							Text => Ada.Strings.Unbounded.Null_Unbounded_String));
					end if;
					-- 吸血鬼の会話があらかじめセットされていたら転写
					if not Night_To_Daytime then
						for Rank in Vampire_Role loop
							for I in People_Index loop
								if Village.People.Constant_Reference(I).Element.Role = Rank then
									if Village.People.Constant_Reference(I).Element.Records.Constant_Reference(Village.Today).Element.State /= Died
										and then Village.People.Constant_Reference(I).Element.Records.Constant_Reference(Village.Today - 1).Element.Note /= Ada.Strings.Unbounded.Null_Unbounded_String
									then
										Night_Talk (
											Village,
											I,
											+Village.People.Constant_Reference (I).Element.Records.Constant_Reference (Village.Today - 1).Element.Note,
											Now);
									end if;
									exit;
								end if;
							end loop;
						end loop;
					end if;
				end if;
				-- 夜から昼へ
				if Night_To_Daytime then
					Village.Time := Tabula.Villages.Daytime;
					-- 誰が処刑されたかを拾っておく
					Executed := -1;
					for I in reverse Village.Messages.First_Index .. Village.Messages.Last_Index loop
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
					end loop;
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
										Target => -1,
										Text => Ada.Strings.Unbounded.Null_Unbounded_String));
								when others => null;
							end case;
						end if;
					end loop;
					-- 観測 (感染すると無効)
					declare
						The_Astronomer : constant Integer := Find_Superman(Village, Astronomer);
					begin
						if The_Astronomer >= 0 and then Village.People.Constant_Reference(The_Astronomer).Element.Records.Constant_Reference(Village.Today).Element.State = Normal then
							declare
								Target : constant Integer := Village.People.Constant_Reference(The_Astronomer).Element.Records.Constant_Reference(Village.Today - 1).Element.Target;
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
							Vampire : constant Integer := Find_Superman(Village, Rank);
						begin
							if Vampire >= 0 and then Village.People.Constant_Reference(Vampire).Element.Records.Constant_Reference(Village.Today).Element.State /= Died then
								if Village.People.Constant_Reference(Vampire).Element.Records.Constant_Reference(Village.Today - 1).Element.Note /= Ada.Strings.Unbounded.Null_Unbounded_String then
									Append(Village.Messages, (
										Kind => Meeting,
										Day => Village.Today,
										Time => Now,
										Subject => -1,
										Target => -1,
										Text => Ada.Strings.Unbounded.Null_Unbounded_String));
									exit Vampire_Meeting;
								end if;
							end if;
						end;
					end loop Vampire_Meeting;
					-- 襲撃 (感染すると天文家、猟師または医者も)、と、ガード (感染すると無効)
					declare
						The_Hunter : constant Integer := Find_Superman(Village, Hunter);
						Guard : Integer := -1;
						Guard_Succeed : Boolean := False;
						Silver_Bullet : Boolean := False;
						procedure Attack(Role : Person_Role; Night_State : Person_State; Attacked : out Boolean) is
							The_Vampire : constant Integer := Find_Superman(Village, Role);
						begin
							Attacked := False;
							if The_Vampire >= 0 and then Village.People.Constant_Reference(The_Vampire).Element.Records.Constant_Reference(Village.Today).Element.State /= Died and then
								(Role in Vampire_Role or else Night_State = Infected)
							then
								declare
									Target : Integer := Village.People.Constant_Reference(The_Vampire).Element.Records.Constant_Reference(Village.Today - 1).Element.Target;
									Result : Message_Kind := Vampire_Infection;
									Hunter_Result : Message_Kind;
								begin
									if Role not in Vampire_Role and then (Target = 0 or else Executed = Target) then
										-- 吸血鬼以外は意図的襲撃失敗はできない
										-- 対象がセットされていない or 処刑対象を選んでいた場合、未感染者を優先で襲う
										loop
											Target := People_Random.Random(Generator);
											exit when Target /= The_Vampire
												and then Village.People.Constant_Reference(Target).Element.Role not in Vampire_Role
												and then Village.People.Constant_Reference(Target).Element.Role /= Gremlin
												and then Village.People.Constant_Reference(Target).Element.Records.Constant_Reference(Village.Today).Element.State = Normal;
										end loop;
									end if;
									if Target >= 0 and then Village.People.Constant_Reference(Target).Element.Records.Constant_Reference(Village.Today).Element.State /= Died then
										Attacked := True;
										if Role in Vampire_Role then
											-- 吸血鬼の襲撃タイプ選択
											case Village.Attack is
												when Two | Mocturnal_Infecting =>
													for Rank2 in Person_Role'Succ(Role) .. Vampire_J loop
														declare
															Vampire2 : constant Integer := Find_Superman(Village, Rank2);
														begin
															if Vampire2 >= 0
																and then Village.People.Constant_Reference(Vampire2).Element.Records.Constant_Reference(Village.Today).Element.State /= Died
																and then Target = Village.People.Constant_Reference(Vampire2).Element.Records.Constant_Reference(Village.Today - 1).Element.Target
															then
																Result := Vampire_Murder;
															end if;
														end;
													end loop;
												when Unanimity =>
													Result := Vampire_Murder;
													for Rank2 in Vampire_Role loop
														declare
															Vampire2 : constant Integer := Find_Superman(Village, Rank2);
														begin
															if Vampire2 >= 0
																and then Village.People.Constant_Reference(Vampire2).Element.Records.Constant_Reference(Village.Today).Element.State /= Died
																and then Target /= Village.People.Constant_Reference(Vampire2).Element.Records.Constant_Reference(Village.Today - 1).Element.Target
															then
																Result := Vampire_Infection;
															end if;
														end;
													end loop;
											end case;
											-- 数奇な運命の村人は常に感染
											if Village.Unfortunate = Infected_Only and then Village.People.Constant_Reference(Target).Element.Role = Unfortunate_Inhabitant then
												Result := Vampire_Infection;
											end if;
										end if;
										if Guard /= Target then
											-- 護衛失敗
											if Village.People.Constant_Reference(Target).Element.Role = Gremlin then
												-- 妖魔
												Result := Vampire_Failed;
											else
												if Result = Vampire_Murder then
													Village.People.Reference(Target).Element.Records.Reference(Village.Today).Element.State := Died;
													Hunter_Result := Hunter_Killed_With_Silver;
												else
													if Village.People.Constant_Reference(Target).Element.Role not in Vampire_Role then
														Village.People.Reference(Target).Element.Records.Reference(Village.Today).Element.State := Infected;
													end if;
													if Guard >= 0 then
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
													Village.People.Reference(The_Vampire).Element.Records.Reference(Village.Today).Element.State := Died;
													Append(Village.Messages, (
														Kind => Hunter_Result, 
														Day => Village.Today,
														Time => Now,
														Subject => The_Hunter,
														Target => Guard,
														Text => Ada.Strings.Unbounded.Null_Unbounded_String));
													Result := Message_Kind'Succ(Result);
												end if;
											end if;
										else
											-- 護衛成功
											Guard_Succeed := True;
											if Silver_Bullet then
												Silver_Bullet := False;
												Village.People.Reference(The_Vampire).Element.Records.Reference(Village.Today).Element.State := Died;
												Hunter_Result := Hunter_Guard_With_Silver;
												Result := Vampire_Failed_And_Killed;
											else
												Hunter_Result := Hunter_Guard;
												Result := Vampire_Failed;
											end if;
											Append(Village.Messages, (
												Kind => Hunter_Result, 
												Day => Village.Today,
												Time => Now,
												Subject => The_Hunter,
												Target => Target,
												Text => Ada.Strings.Unbounded.Null_Unbounded_String));
										end if;
										Append(Village.Messages, (
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
						Attacked : Boolean;
						Astronomer_State : Person_State;
						Hunter_State : Person_State;
					begin
						-- 猟師の行動確認
						if The_Hunter >= 0 and then Village.People.Constant_Reference(The_Hunter).Element.Records.Constant_Reference(Village.Today).Element.State = Normal then
							Guard := Village.People.Constant_Reference(The_Hunter).Element.Records.Constant_Reference(Village.Today - 1).Element.Target;
							Silver_Bullet := Village.People.Constant_Reference(The_Hunter).Element.Records.Constant_Reference(Village.Today - 1).Element.Special;
						end if;
						-- 連鎖が起きないようにここで状態を保存
						if The_Hunter >= 0 then
							Hunter_State := Village.People.Constant_Reference(The_Hunter).Element.Records.Constant_Reference(Village.Today).Element.State;
						end if;
						declare
							The_Astronomer : constant Integer := Find_Superman(Village, Astronomer);
						begin
							if The_Astronomer >= 0 then
								Astronomer_State := Village.People.Constant_Reference(The_Astronomer).Element.Records.Constant_Reference(Village.Today).Element.State;
							end if;
						end;
						-- 吸血鬼の襲撃
						for Rank in Vampire_Role loop
							Attack(Rank, Infected, Attacked);
							exit when Attacked;
						end loop;
						case Village.Attack is
							when Mocturnal_Infecting =>
								-- 天文家の襲撃
								Attack(Astronomer, Astronomer_State, Attacked);
								-- 猟師の襲撃
								Attack(Hunter, Hunter_State, Attacked);
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
									Target => -1,
									Text => Ada.Strings.Unbounded.Null_Unbounded_String));
							end if;
						end if;
					end;
					-- 妖魔
					declare
						The_Gremlin : constant Integer := Find_Superman(Village, Gremlin);
					begin
						if The_Gremlin >= 0 and then Village.People.Constant_Reference(The_Gremlin).Element.Records.Constant_Reference(Village.Today).Element.State /= Died then
							Append(Village.Messages, (
								Kind => Gremlin_Sense, 
								Day => Village.Today,
								Time => Now,
								Subject => The_Gremlin,
								Target => -1,
								Text => Ada.Strings.Unbounded.Null_Unbounded_String));
						end if;
					end;
					if Gremlin_Kill_Doctor then
						declare
							The_Doctor : constant Integer := Find_Superman(Village, Doctor);
						begin
							pragma Assert(The_Doctor >= 0);
							Village.People.Reference(The_Doctor).Element.Records.Reference(Village.Today).Element.State := Died;
						end;
					end if;
					if Gremlin_Kill_Astronomer then
						declare
							The_Astronomer : constant Integer := Find_Superman(Village, Astronomer);
						begin
							pragma Assert(The_Astronomer >= 0);
							Village.People.Reference(The_Astronomer).Element.Records.Reference(Village.Today).Element.State := Died;
						end;
					end if;
					-- 恋人
					declare
						procedure Love_Process(Lover_Role, Loved_Role : Person_Role) is
							Lover_Person : constant Integer := Find_Superman(Village, Lover_Role);
							Loved_Person : constant Integer := Find_Superman(Village, Loved_Role);
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
						Subject => -1, 
						Target => -1,
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
						Target : Integer := -1;
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
						Village.State := Tabula.Villages.Epilogue;
						-- 全員コミット解除
						for I in People_Index loop
							Village.People.Reference(I).Element.Commited := False;
						end loop;
					end if;
				end if;
			end;
		when Tabula.Villages.Epilogue =>
			Changed := (Now - Village.Dawn >= Village.Day_Duration)
				and then (Now - Village.Dawn >= Epilogue_Min_Duration); --  エピローグは最低でも1時間
			List_Changed := Changed;
			if Changed then
				Village.Dawn := Now;
				Village.State := Tabula.Villages.Closed;
			end if;
		when Tabula.Villages.Closed =>
			Changed := False;
			List_Changed := False;
	end case;
end Tabula.Vampires.Villages.Advance;
