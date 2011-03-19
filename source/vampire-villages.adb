-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Containers.Generic_Array_Sort;
package body Vampire.Villages is
	use People;
	use type Ada.Calendar.Time;
	use type Ada.Strings.Unbounded.Unbounded_String;
	use type Casts.Person_Sex;
	
	function Provisional_Voting (Mode : Execution_Mode) return Boolean is
	begin
		case Mode is
			when Provisional_Voting_From_First | Provisional_Voting_From_Second =>
				return True;
			when Dummy_Killed_And_From_First | From_First | From_Second =>
				return False;
		end case;
	end Provisional_Voting;
	
	function Be_Voting (Village : Village_Type) return Boolean is
	begin
		if Village.State = Playing then
			case Village.Execution is
				when Dummy_Killed_And_From_First | From_First | Provisional_Voting_From_First =>
					return True;
				when From_Second | Provisional_Voting_From_Second =>
					return Village.Today >= 2;
			end case;
		else
			return False;
		end if;
	end Be_Voting;
	
	function Count_Messages(Village : Village_Type; Day : Natural) return Message_Counts is
		Result : Message_Counts(Village.Messages.First_Index .. Village.Messages.Last_Index) := (others => (
			Speech => 0, Monologue => 0, Ghost => 0, Wake => 0, Encourage => 0, Encouraged => 0, Vampire_Gaze => 0, 
			Last_Action_Time => Calendar.Null_Time));
	begin
		for Position in Village.Messages.First_Index .. Village.Messages.Last_Index loop
			declare
				Current : Message renames Village.Messages.Constant_Reference(Position).Element.all;
			begin
				if Current.Day = Day then
					case Current.Kind is
						when Join =>
							Result(Current.Subject).Last_Action_Time := Current.Time;
						when Speech =>
							Result(Current.Subject).Speech := Result(Current.Subject).Speech + 1;
							Result(Current.Subject).Last_Action_Time := Current.Time;
						when Escaped_Speech =>
							declare
								Rejoined : constant Person_Index'Base := Village.Joined (
									Village.Escaped_People.Constant_Reference (Current.Subject).Element.
										Id.Constant_Reference.Element.all);
							begin
								if Rejoined /= No_Person
									and then Same_Id_And_Figure (
										Village.Escaped_People.Constant_Reference (Current.Subject).Element.all,
										Village.People.Constant_Reference (Rejoined).Element.all)
								then
									Result (Rejoined).Speech := Result (Rejoined).Speech + 1;
								end if;
							end;
						when Monologue =>
							Result(Current.Subject).Monologue := Result(Current.Subject).Monologue + 1;
						when Ghost =>
							Result(Current.Subject).Ghost := Result(Current.Subject).Ghost + 1;
						when Action_Wake =>
							Result(Current.Subject).Wake := Result(Current.Subject).Wake + 1;
						when Action_Encourage =>
							Result(Current.Subject).Encourage := Result(Current.Subject).Encourage + 1;
							Result(Current.Target).Encouraged := Result(Current.Target).Encouraged + 1;
						when Action_Vampire_Gaze | Action_Vampire_Gaze_Blocked =>
							Result(Current.Subject).Vampire_Gaze := Result(Current.Subject).Vampire_Gaze + 1;
						when others =>
							null;
					end case;
				end if;
			end;
		end loop;
		return Result;
	end Count_Messages;
	
	function Count_Total_Speech (Village : Village_Type; Day : Natural) return Natural is
		Result : Natural := 0;
	begin
		for Position in Village.Messages.First_Index .. Village.Messages.Last_Index loop
			declare
				Current : Message renames Village.Messages.Constant_Reference(Position).Element.all;
			begin
				if Current.Day = Day and then (Current.Kind = Speech or else Current.Kind = Escaped_Speech) then
					Result := Result + 1;
				end if;
			end;
		end loop;
		return Result;
	end Count_Total_Speech;
	
	function No_Commit (Village : Village_Type) return Boolean is
	begin
		for I in Village.People.First_Index .. Village.People.Last_Index loop
			if Village.People.Constant_Reference(I).Element.Records.Constant_Reference(Village.Today).Element.State /= Died then
				if Village.People.Constant_Reference(I).Element.Commited then
					return False;
				end if;
			end if;
		end loop;
		return True;
	end No_Commit;
	
	function Commit_Finished(Village : Village_Type) return Boolean is
		Count : Integer := 0;
		Commited_Count : Integer := 0;
	begin
		for I in Village.People.First_Index .. Village.People.Last_Index loop
			if Village.People.Constant_Reference(I).Element.Records.Constant_Reference(Village.Today).Element.State /= Died then
				Count := Count + 1;
				if Village.People.Constant_Reference(I).Element.Commited then
					Commited_Count := Commited_Count + 1;
				end if;
			end if;
		end loop;
		return (Count >= Minimum_Number_Of_Persons or else Village.State /= Prologue)
			and then Commited_Count = Count;
	end Commit_Finished;
	
	function Provisional_Voted (Village : Village_Type) return Boolean is
	begin
		for I in reverse Village.Messages.First_Index .. Village.Messages.Last_Index loop
			declare
				It : Message renames Village.Messages.Constant_Reference (I).Element.all;
			begin
				exit when It.Day /= Village.Today;
				if It.Kind = Provisional_Vote then
					return True;
				end if;
			end;
		end loop;
		return False;
	end Provisional_Voted;
	
	function Vote_Finished(Village : Village_Type) return Boolean is
	begin
		for I in Village.People.First_Index .. Village.People.Last_Index loop
			declare
				P : Person_Type renames Village.People.Constant_Reference(I).Element.all;
			begin
				if P.Records.Constant_Reference(Village.Today).Element.State /= Died
					and then not P.Commited
					and then P.Records.Constant_Reference(Village.Today).Element.Vote < 0
				then
					return False;
				end if;
			end;
		end loop;
		return True;
	end Vote_Finished;
	
	function Voted_Count (Village : Village_Type; Day : Natural; Provisional : Boolean) return Voted_Count_Info is
	begin
		return Result : Voted_Count_Info := (
			Last => Village.People.Last_Index,
			Max => 0,
			Counts => (Village.People.First_Index .. Village.People.Last_Index => 0))
		do
			for I in Result.Counts'Range loop
				declare
					P : Person_Type renames Village.People.Constant_Reference (I).Element.all;
					V : Integer;
				begin
					if Provisional then
						V := P.Records.Constant_Reference (Day).Element.Provisional_Vote;
					else
						V := P.Records.Constant_Reference (Day).Element.Vote;
					end if;
					if V in Result.Counts'Range then
						Result.Counts (V) := Result.Counts (V) + 1;
						if Result.Counts (V) > Result.Max then
							Result.Max := Result.Counts (V);
						end if;
					end if;
				end;
			end loop;
		end return;
	end Voted_Count;
	
	function Night_To_Daytime (Village : Village_Type) return Ada.Calendar.Time is
	begin
		return Village.Dawn + Village.Night_Duration;
	end Night_To_Daytime;
	
	function Provisional_Voting_Time (Village : Village_Type) return Ada.Calendar.Time is
	begin
		if Village.Today = 1 and then Village.Execution = Provisional_Voting_From_First then
			return Village.Night_To_Daytime + Village.Day_Duration; -- 48h
		else
			return Village.Night_To_Daytime + Village.Day_Duration / 2; -- 24h
		end if;
	end Provisional_Voting_Time;
	
	function Daytime_To_Vote (Village : Village_Type) return Ada.Calendar.Time is
	begin
		if Village.Today = 1 and then Village.Execution = Provisional_Voting_From_First then
			return Village.Night_To_Daytime + Village.Day_Duration * 3 / 2; -- 72h
		else
			return Village.Night_To_Daytime + Village.Day_Duration; -- 48h
		end if;
	end Daytime_To_Vote;
	
	function Vote_To_Night (Village : Village_Type) return Ada.Calendar.Time is
	begin
		return Village.Daytime_To_Vote + Vote_Duration;
	end Vote_To_Night;
	
	function Find_Superman(Village : Village_Type; Role : Person_Role) return Integer is
	begin
		for I in Village.People.First_Index .. Village.People.Last_Index loop
			if Village.People.Constant_Reference(I).Element.Role = Role then
				return I;
			end if;
		end loop;
		return -1;
	end Find_Superman;
	
	function Unfortunate(Village : Village_Type) return Boolean is
		The_Unfortunate_Inhabitant : constant Integer := Find_Superman(Village, Unfortunate_Inhabitant);
	begin
		if The_Unfortunate_Inhabitant < 0 then
			return False;
		else
			for I in 1 .. Village.Today loop
				if Village.People.Constant_Reference(The_Unfortunate_Inhabitant).Element.Records.Constant_Reference(I).Element.State /= Normal then
					return False;
				end if;
			end loop;
			if Village.Today > 1 then
				declare
					Counts : Message_Counts renames Count_Messages(Village, Village.Today - 1);
				begin
					if Counts(The_Unfortunate_Inhabitant).Speech = 0 then
						return False;
					end if;
				end;
			end if;
			return True;
		end if;
	end Unfortunate;
	
	procedure Escape (
		Village : in out Village_Type;
		Subject : Natural;
		Time : Ada.Calendar.Time)
	is
		Escaped_Index : Natural;
	begin
		People.Append (Village.Escaped_People, Village.People.Constant_Reference (Subject).Element.all);
		People.Delete (Village.People, Subject);
		Escaped_Index := Village.Escaped_People.Last_Index;
		for I in Village.Messages.First_Index .. Village.Messages.Last_Index loop
			declare
				Kind : constant Villages.Message_Kind :=
					Village.Messages.Constant_Reference (I).Element.Kind;
			begin
				if Kind = Escaped_Join then
					null;
				elsif Kind = Escaped_Speech then
					null;
				elsif Kind = Escape then
					null;
				elsif Kind = Speech and then Village.Messages.Constant_Reference (I).Element.Subject = Subject then
					Village.Messages.Reference (I).Element.Kind := Escaped_Speech;
					Village.Messages.Reference (I).Element.Subject := Escaped_Index;
				elsif Kind = Join and then Village.Messages.Constant_Reference (I).Element.Subject = Subject then
					Village.Messages.Reference (I).Element.Kind := Escaped_Join;
					Village.Messages.Reference (I).Element.Subject := Escaped_Index;
				elsif Village.Messages.Constant_Reference (I).Element.Subject > Subject then
					Village.Messages.Reference (I).Element.Subject :=
						Village.Messages.Constant_Reference (I).Element.Subject - 1;
				end if;
			end;
		end loop;
		Messages.Append (Village.Messages, Message'(
			Kind => Escape,
			Day => Village.Today,
			Time => Time,
			Subject => Escaped_Index,
			Target => No_Index,
			Text => Ada.Strings.Unbounded.Null_Unbounded_String));
	end Escape;
	
	procedure Vote (
		Village : in out Village_Type;
		Player : in Natural;
		Target : in Integer)
	is
		Rec : Person_Record renames Village.People.Reference(Player).Element.Records.Reference(Village.Today).Element.all;
	begin
		pragma Assert(Target < 0 or else Village.People.Constant_Reference(Target).Element.Records.Constant_Reference(Village.Today).Element.Candidate);
		Rec.Vote := Target;
		if not Provisional_Voted(Village)
			and then Village.Time = Daytime -- 短期の投票延長期間は仮投票は発生させない
		then
			Rec.Provisional_Vote := Target;
		end if;
	end Vote;
	
	procedure Provisional_Vote (
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
					Target : Integer := Village.People.Constant_Reference (I).Element.Records.Constant_Reference (Village.Today).Element.Provisional_Vote;
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
					V : Integer renames Village.People.Reference (I).Element.Records.Reference (Village.Today).Element.Vote;
				begin
					if V >= 0 and then not Village.People.Constant_Reference (V).Element.Records.Constant_Reference (Village.Today).Element.Candidate then
						V := -1;
					end if;
				end;
			end loop;
			Messages.Append (Village.Messages, Message'(
				Kind => Provisional_Vote,
				Day => Village.Today,
				Time => Time,
				Subject => -1,
				Target => -1,
				Text => Ada.Strings.Unbounded.Null_Unbounded_String));
			Changed := True; -- 変更を保存
		end if;
	end Provisional_Vote;
	
	function Escape_Duration(Village : Village_Type) return Duration is
	begin
		case Village.Term is
			when Short => return 12 * 60 * 60 * 1.0;
			when Long => return 7 * 24 * 60 * 60 * 1.0;
		end case;
	end Escape_Duration;
	
	procedure Night_Talk (
		Village : in out Village_Type;
		Player : in Natural;
		Text : in String;
		Time : in Ada.Calendar.Time) is
	begin
		if Unfortunate (Village) then
			for I in reverse Village.Messages.First_Index .. Village.Messages.Last_Index loop
				declare
					Message : Villages.Message renames Village.Messages.Constant_Reference (I).Element.all;
				begin
					if Message.Day < Village.Today then
						Messages.Append (Village.Messages, Villages.Message'(
							Kind => Howling_Blocked,
							Day => Village.Today,
							Time => Time,
							Subject => -1,
							Target => -1,
							Text => Ada.Strings.Unbounded.Null_Unbounded_String));
						exit;
					end if;
					exit when Message.Kind = Howling_Blocked;
				end;
			end loop;
			-- 夜明け時に1発言はできるので記録しておく
			Village.People.Reference (Player).Element.Records.Reference (Village.Today - 1).Element.Note := +Text;
		else
			Messages.Append (Village.Messages, Message'(
				Kind => Howling,
				Day => Village.Today,
				Time => Time,
				Subject => Player,
				Target => -1,
				Text => +Text));
			Village.People.Reference (Player).Element.Records.Reference (Village.Today - 1).Element.Note := Ada.Strings.Unbounded.Null_Unbounded_String;
		end if;
	end Night_Talk;
	
	overriding function Term (Village : Village_Type) return Village_Term is
	begin
		if Village.Day_Duration < 24 * 60 * 60.0 then
			return Short;
		else
			return Long;
		end if;
	end Term;
	
	overriding procedure Get_State (
		Village : in Village_Type;
		State : out Village_State;
		Today : out Natural) is
	begin
		State := Village.State;
		Today := Village.Today;
	end Get_State;
	
	overriding procedure Iterate_People (
		Village : in Village_Type;
		Process : not null access procedure (
			Index : Person_Index;
			Item : in Tabula.Villages.Person_Type'Class)) is
	begin
		for I in Village.People.First_Index .. Village.People.Last_Index loop
			Process (I, Village.People.Constant_Reference (I).Element.all);
		end loop;
	end Iterate_People;
	
	overriding procedure Iterate_Escaped_People (
		Village : in Village_Type;
		Process : not null access procedure (
			Index : Person_Index;
			Item : in Tabula.Villages.Person_Type'Class)) is
	begin
		for I in Village.Escaped_People.First_Index .. Village.Escaped_People.Last_Index loop
			Process (I, Village.People.Constant_Reference (I).Element.all);
		end loop;
	end Iterate_Escaped_People;
	
	overriding procedure Iterate_Options (
		Village : in Village_Type;
		Process : not null access procedure (Item : in Root_Option_Item'Class)) is
	begin
		Process (Options.Day_Duration.Option_Item'(Village => Village'Access));
		Process (Options.Night_Duration.Option_Item'(Village => Village'Access));
		Process (Options.Execution.Option_Item'(Village => Village'Access));
		Process (Options.Teaming.Option_Item'(Village => Village'Access));
		Process (Options.Monster_Side.Option_Item'(Village => Village'Access));
		Process (Options.Attack.Option_Item'(Village => Village'Access));
		Process (Options.Servant_Knowing.Option_Item'(Village => Village'Access));
		Process (Options.Daytime_Preview.Option_Item'(Village => Village'Access));
		Process (Options.Doctor_Infected.Option_Item'(Village => Village'Access));
		Process (Options.Hunter_Silver_Bullet.Option_Item'(Village => Village'Access));
		Process (Options.Unfortunate.Option_Item'(Village => Village'Access));
	end Iterate_Options;
	
	package body Options is
		
		package body Day_Duration is
			
			overriding function Available (Item : Option_Item) return Boolean is
			begin
				return Item.Village.Term = Short;
			end Available;
			
			overriding function Name (Item : Option_Item) return String is
			begin
				return "day-duration";
			end Name;
			
			overriding function Changed (Item : Option_Item) return Boolean is
			begin
				return True;
			end Changed;
			
			overriding procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean)) is
			begin
				Process (
					Duration'Image (15 * 60.0),
					Item.Village.Day_Duration = 15 * 60.0,
					"ゲームの1日は実時間の15分です。",
					False);
				Process (
					Duration'Image (20 * 60.0),
					Item.Village.Day_Duration = 20 * 60.0,
					"ゲームの1日は実時間の20分です。",
					False);
				Process (
					Duration'Image (25 * 60.0),
					Item.Village.Day_Duration = 25 * 60.0,
					"ゲームの1日は実時間の25分です。",
					False);
				Process (
					Duration'Image (30 * 60.0),
					Item.Village.Day_Duration = 30 * 60.0,
					"ゲームの1日は実時間の30分です。",
					False);
			end Iterate;
			
			overriding procedure Change (
				Village : in out Tabula.Villages.Village_Type'Class;
				Item : in Option_Item;
				Value : in String)
			is
				V : Village_Type renames Village_Type (Village);
			begin
				pragma Assert (V'Access = Item.Village);
				if Value /= "" and then Available (Item) then
					V.Day_Duration := Duration'Value (Value);
				end if;
			end Change;
			
		end Day_Duration;
		
		package body Night_Duration is
			
			overriding function Available (Item : Option_Item) return Boolean is
			begin
				return Item.Village.Term = Short;
			end Available;
			
			overriding function Name (Item : Option_Item) return String is
			begin
				return "night-duration";
			end Name;
			
			overriding function Changed (Item : Option_Item) return Boolean is
			begin
				return True;
			end Changed;
			
			overriding procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean)) is
			begin
				Process (
					Duration'Image (0 * 60.0),
					Item.Village.Night_Duration = 0 * 60.0,
					"夜はありません。",
					False);
				Process (
					Duration'Image (5 * 60.0),
					Item.Village.Night_Duration = 5 * 60.0,
					"夜は5分です。",
					False);
			end Iterate;
			
			overriding procedure Change (
				Village : in out Tabula.Villages.Village_Type'Class;
				Item : in Option_Item;
				Value : in String)
			is
				V : Village_Type renames Village_Type (Village);
			begin
				pragma Assert (V'Access = Item.Village);
				if Value /= "" and then Available (Item) then
					V.Night_Duration := Duration'Value (Value);
				end if;
			end Change;
			
		end Night_Duration;
		
		package body Execution is
			
			overriding function Available (Item : Option_Item) return Boolean is
			begin
				return True;
			end Available;
			
			overriding function Name (Item : Option_Item) return String is
			begin
				return "execution";
			end Name;
			
			overriding function Changed (Item : Option_Item) return Boolean is
			begin
				return Item.Village.Execution /= Initial_Execution;
			end Changed;
			
			overriding procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean)) is
			begin
				Process (
					Execution_Mode'Image (Dummy_Killed_And_From_First),
					Item.Village.Execution = Dummy_Killed_And_From_First,
					"能力者が死亡済みの可能性があり、無記名投票で処刑を行います。",
					False);
				Process (
					Execution_Mode'Image (From_First),
					Item.Village.Execution = From_First,
					"初日から無記名投票で処刑を行います。",
					False);
				Process (
					Execution_Mode'Image (Provisional_Voting_From_First),
					Item.Village.Execution = Provisional_Voting_From_First,
					"初日から仮投票と本投票で処刑を行います。",
					False);
				Process (
					Execution_Mode'Image (From_Second),
					Item.Village.Execution = From_Second,
					"2日目から無記名投票で処刑を行います。",
					False);
				Process (
					Execution_Mode'Image (Provisional_Voting_From_Second),
					Item.Village.Execution = Provisional_Voting_From_Second,
					"2日目から仮投票と本投票で処刑を行います。",
					False);
			end Iterate;
			
			overriding procedure Change (
				Village : in out Tabula.Villages.Village_Type'Class;
				Item : in Option_Item;
				Value : in String)
			is
				V : Village_Type renames Village_Type (Village);
			begin
				pragma Assert (V'Access = Item.Village);
				V.Execution := Execution_Mode'Value (Value);
			end Change;
			
		end Execution;
		
		package body Teaming is
			
			overriding function Available (Item : Option_Item) return Boolean is
			begin
				return True;
			end Available;
			
			overriding function Name (Item : Option_Item) return String is
			begin
				return "teaming";
			end Name;
			
			overriding function Changed (Item : Option_Item) return Boolean is
			begin
				return Item.Village.Teaming /= Initial_Teaming;
			end Changed;
			
			overriding procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean)) is
			begin
				Process (
					Teaming_Mode'Image (Low_Density),
					Item.Village.Teaming = Low_Density,
					"能力者の密度を線形にします。",
					False);
				Process (
					Teaming_Mode'Image (Shuffling_Headless),
					Item.Village.Teaming = Shuffling_Headless,
					"村側能力者を増やします(首無し騎士に似せます)。",
					Unrecommended => True);
				Process (
					Teaming_Mode'Image (Shuffling_Euro),
					Item.Village.Teaming = Shuffling_Euro,
					"妖魔が早く出ます(欧州に似せます)。",
					False);
				Process (
					Teaming_Mode'Image (Shuffling),
					Item.Village.Teaming = Shuffling,
					"基本的な編成です。",
					False);
				Process (
					Teaming_Mode'Image (Shuffling_Gremlin),
					Item.Village.Teaming = Shuffling_Gremlin,
					"基本的な編成に加え、妖魔が早く出ます。",
					False);
				Process (
					Teaming_Mode'Image (Hiding),
					Item.Village.Teaming = Hiding,
					"村側能力者の構成はわかりません。",
					False);
				Process (
					Teaming_Mode'Image (Hiding_Gremlin),
					Item.Village.Teaming = Hiding_Gremlin,
					"村側能力者の構成はわからず、妖魔が早く出ます。",
					False);
			end Iterate;
			
			overriding procedure Change (
				Village : in out Tabula.Villages.Village_Type'Class;
				Item : in Option_Item;
				Value : in String)
			is
				V : Village_Type renames Village_Type (Village);
			begin
				pragma Assert (V'Access = Item.Village);
				V.Teaming := Teaming_Mode'Value (Value);
			end Change;
			
		end Teaming;
		
		package body Monster_Side is
			
			overriding function Available (Item : Option_Item) return Boolean is
			begin
				return True;
			end Available;
			
			overriding function Name (Item : Option_Item) return String is
			begin
				return "monster-side";
			end Name;
			
			overriding function Changed (Item : Option_Item) return Boolean is
			begin
				return Item.Village.Monster_Side /= Initial_Monster_Side;
			end Changed;
			
			overriding procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean)) is
			begin
				Process (
					Monster_Side_Mode'Image (Fixed),
					Item.Village.Monster_Side = Fixed,
					"吸血鬼が襲ってきます。",
					False);
				Process (
					Monster_Side_Mode'Image (Shuffling),
					Item.Village.Monster_Side = Shuffling,
					"吸血鬼の全貌はわかりません。",
					Unrecommended => True);
				Process (
					Monster_Side_Mode'Image (Gremlin),
					Item.Village.Monster_Side = Gremlin,
					"使徒よりも妖魔が先に現れます。",
					False);
			end Iterate;
			
			overriding procedure Change (
				Village : in out Tabula.Villages.Village_Type'Class;
				Item : in Option_Item;
				Value : in String)
			is
				V : Village_Type renames Village_Type (Village);
			begin
				pragma Assert (V'Access = Item.Village);
				V.Monster_Side := Monster_Side_Mode'Value (Value);
			end Change;
			
		end Monster_Side;
		
		package body Attack is
			
			overriding function Available (Item : Option_Item) return Boolean is
			begin
				return True;
			end Available;
			
			overriding function Name (Item : Option_Item) return String is
			begin
				return "attack";
			end Name;
			
			overriding function Changed (Item : Option_Item) return Boolean is
			begin
				return Item.Village.Attack /= Initial_Attack;
			end Changed;
			
			overriding procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean)) is
			begin
				Process (
					Attack_Mode'Image (Two),
					Item.Village.Attack = Two,
					"吸血鬼ふたり以上に襲われると死亡します。",
					False);
				Process (
					Attack_Mode'Image (Mocturnal_Infecting),
					Item.Village.Attack = Mocturnal_Infecting,
					"天文家と猟師は感染したら襲撃を行います。",
					False);
				Process (
					Attack_Mode'Image (Unanimity),
					Item.Village.Attack = Unanimity,
					"すべての吸血鬼に襲われると死亡します。",
					False);
			end Iterate;
			
			overriding procedure Change (
				Village : in out Tabula.Villages.Village_Type'Class;
				Item : in Option_Item;
				Value : in String)
			is
				V : Village_Type renames Village_Type (Village);
			begin
				pragma Assert (V'Access = Item.Village);
				V.Attack := Attack_Mode'Value (Value);
			end Change;
			
		end Attack;
		
		package body Servant_Knowing is
			
			overriding function Available (Item : Option_Item) return Boolean is
			begin
				return True;
			end Available;
			
			overriding function Name (Item : Option_Item) return String is
			begin
				return "servant-knowing";
			end Name;
			
			overriding function Changed (Item : Option_Item) return Boolean is
			begin
				return Item.Village.Servant_Knowing /= Initial_Servant_Knowing;
			end Changed;
			
			overriding procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean)) is
			begin
				Process (
					Servant_Knowing_Mode'Image (None),
					Item.Village.Servant_Knowing = None,
					"使徒は吸血鬼の正体を知りません。",
					False);
				Process (
					Servant_Knowing_Mode'Image (Vampire_K),
					Item.Village.Servant_Knowing = Vampire_K,
					"使徒は吸血鬼の王を知っています。",
					False);
				Process (
					Servant_Knowing_Mode'Image (All_Vampires),
					Item.Village.Servant_Knowing = All_Vampires,
					"使徒は吸血鬼を知っています。",
					False);
			end Iterate;
			
			overriding procedure Change (
				Village : in out Tabula.Villages.Village_Type'Class;
				Item : in Option_Item;
				Value : in String)
			is
				V : Village_Type renames Village_Type (Village);
			begin
				pragma Assert (V'Access = Item.Village);
				V.Servant_Knowing := Servant_Knowing_Mode'Value (Value);
			end Change;
			
		end Servant_Knowing;
		
		package body Daytime_Preview is
			
			overriding function Available (Item : Option_Item) return Boolean is
			begin
				return True;
			end Available;
			
			overriding function Name (Item : Option_Item) return String is
			begin
				return "daytime-preview";
			end Name;
			
			overriding function Changed (Item : Option_Item) return Boolean is
			begin
				return Item.Village.Daytime_Preview /= Initial_Daytime_Preview;
			end Changed;
			
			overriding procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean)) is
			begin
				Process (
					Daytime_Preview_Mode'Image (None),
					Item.Village.Daytime_Preview = None,
					"探偵と医者は翌日まで結果がわかりません。",
					Unrecommended => True);
				Process (
					Daytime_Preview_Mode'Image (Role_Only),
					Item.Village.Daytime_Preview = Role_Only,
					"探偵は日中に正体を調べ翌日までに遺言を調べます。",
					False);
				Process (
					Daytime_Preview_Mode'Image (Message_Only),
					Item.Village.Daytime_Preview = Message_Only,
					"探偵は日中に遺言を調べ翌日までに正体を調べます。",
					False);
				Process (
					Daytime_Preview_Mode'Image (Role_And_Message),
					Item.Village.Daytime_Preview = Role_And_Message,
					"探偵は日中に正体と遺言を調べます。",
					False);
			end Iterate;
			
			overriding procedure Change (
				Village : in out Tabula.Villages.Village_Type'Class;
				Item : in Option_Item;
				Value : in String)
			is
				V : Village_Type renames Village_Type (Village);
			begin
				pragma Assert (V'Access = Item.Village);
				V.Daytime_Preview := Daytime_Preview_Mode'Value (Value);
			end Change;
			
		end Daytime_Preview;
		
		package body Doctor_Infected is
			
			overriding function Available (Item : Option_Item) return Boolean is
			begin
				return True;
			end Available;
			
			overriding function Name (Item : Option_Item) return String is
			begin
				return "doctor-infected";
			end Name;
			
			overriding function Changed (Item : Option_Item) return Boolean is
			begin
				return Item.Village.Doctor_Infected /= Initial_Doctor_Infected;
			end Changed;
			
			overriding procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean)) is
			begin
				Process (
					Doctor_Infected_Mode'Image (Cure),
					Item.Village.Doctor_Infected = Cure,
					"医者自身の感染は治療に影響しません。",
					False);
				Process (
					Doctor_Infected_Mode'Image (Find_Infection),
					Item.Village.Doctor_Infected = Find_Infection,
					"医者自身が感染していると治療の効果はありません。",
					False);
			end Iterate;
			
			overriding procedure Change (
				Village : in out Tabula.Villages.Village_Type'Class;
				Item : in Option_Item;
				Value : in String)
			is
				V : Village_Type renames Village_Type (Village);
			begin
				pragma Assert (V'Access = Item.Village);
				V.Doctor_Infected := Doctor_Infected_Mode'Value (Value);
			end Change;
			
		end Doctor_Infected;
		
		package body Hunter_Silver_Bullet is
			
			overriding function Available (Item : Option_Item) return Boolean is
			begin
				return True;
			end Available;
			
			overriding function Name (Item : Option_Item) return String is
			begin
				return "hunter-silver-bullet";
			end Name;
			
			overriding function Changed (Item : Option_Item) return Boolean is
			begin
				return Item.Village.Hunter_Silver_Bullet /= Initial_Hunter_Silver_Bullet;
			end Changed;
			
			overriding procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean)) is
			begin
				Process (
					Hunter_Silver_Bullet_Mode'Image (Target),
					Item.Village.Hunter_Silver_Bullet = Target,
					"護衛対象が襲われたときのみ銀の弾丸は吸血鬼を殺します。",
					False);
				Process (
					Hunter_Silver_Bullet_Mode'Image (Target_And_Self),
					Item.Village.Hunter_Silver_Bullet = Target_And_Self,
					"猟師自身が襲われたときも銀の弾丸は吸血鬼を殺します。",
					False);
			end Iterate;
			
			overriding procedure Change (
				Village : in out Tabula.Villages.Village_Type'Class;
				Item : in Option_Item;
				Value : in String)
			is
				V : Village_Type renames Village_Type (Village);
			begin
				pragma Assert (V'Access = Item.Village);
				V.Hunter_Silver_Bullet := Hunter_Silver_Bullet_Mode'Value (Value);
			end Change;
			
		end Hunter_Silver_Bullet;
		
		package body Unfortunate is
			
			overriding function Available (Item : Option_Item) return Boolean is
			begin
				return True;
			end Available;
			
			overriding function Name (Item : Option_Item) return String is
			begin
				return "unfortunate";
			end Name;
			
			overriding function Changed (Item : Option_Item) return Boolean is
			begin
				return Item.Village.Unfortunate /= Initial_Unfortunate;
			end Changed;
			
			overriding procedure Iterate (
				Item : in Option_Item;
				Process : not null access procedure (
					Value : in String;
					Selected : in Boolean;
					Message : in String;
					Unrecommended : in Boolean)) is
			begin
				Process (
					Unfortunate_Mode'Image (None),
					Item.Village.Unfortunate = None,
					"数奇な運命の村人はいません。",
					False);
				Process (
					Unfortunate_Mode'Image (Appear),
					Item.Village.Unfortunate = Appear,
					"数奇な運命の村人がいるかもしれません。",
					Unrecommended => True);
				Process (
					Unfortunate_Mode'Image (Infected_Only),
					Item.Village.Unfortunate = Infected_Only,
					"数奇な運命の村人は襲撃では殺されません。",
					False);
			end Iterate;
			
			overriding procedure Change (
				Village : in out Tabula.Villages.Village_Type'Class;
				Item : in Option_Item;
				Value : in String)
			is
				V : Village_Type renames Village_Type (Village);
			begin
				pragma Assert (V'Access = Item.Village);
				V.Unfortunate := Unfortunate_Mode'Value (Value);
			end Change;
			
		end Unfortunate;
		
	end Options;

end Vampire.Villages;
