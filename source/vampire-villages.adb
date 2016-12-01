-- The Village of Vampire by YT, このソースコードはNYSLです
package body Vampire.Villages is
	use Messages;
	use Person_Records;
	use People;
	use type Ada.Calendar.Time;
	use type Ada.Strings.Unbounded.Unbounded_String;
	
	function Equivalent_Messages (Left, Right : Message) return Boolean is
	begin
		return Left.Day = Right.Day
			and then abs (Left.Time - Right.Time) < 1.0 -- 一秒未満は記録されないため
			and then Left.Kind = Right.Kind
			and then Left.Subject = Right.Subject
			and then Left.Target = Right.Target
			and then Left.Text = Right.Text;
	end Equivalent_Messages;
	
	function Preliminary_Voted (Village : Village_Type) return Boolean is
	begin
		for I in reverse Village.Messages.First_Index .. Village.Messages.Last_Index loop
			declare
				It : Message renames Village.Messages.Constant_Reference (I);
			begin
				exit when It.Day /= Village.Today;
				if It.Kind = Preliminary_Vote then
					return True;
				end if;
			end;
		end loop;
		return False;
	end Preliminary_Voted;
	
	function Unfortunate (Village : Village_Type) return Boolean is
		The_Unfortunate_Inhabitant : constant Integer := Find_Superman(Village, Unfortunate_Inhabitant);
	begin
		if The_Unfortunate_Inhabitant < 0 then
			return False;
		else
			for I in 1 .. Village.Today loop
				if Village.People.Constant_Reference(The_Unfortunate_Inhabitant).Records.Constant_Reference(I).State /= Normal then
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
	
	-- implementation
	
	function Create (Name : String; By : String; Term : Village_Term; Time : Ada.Calendar.Time)
		return Village_Type
	is
		Day_Duration : Duration;
	begin
		case Term is
			when Short => Day_Duration := Default_Short_Day_Duration;
			when Long => Day_Duration := Default_Long_Day_Duration;
		end case;
		return (
			Name => +Name,
			By => +By,
			Face_Group => 0,
			Face_Width => 0,
			Face_Height => 0,
			State => Prologue,
			Today => 0,
			Time => Daytime,
			Dawn => Time,
			Day_Duration => Day_Duration,
			Night_Duration => Default_Night_Duration,
			Vote => Initial_Vote,
			Execution => Initial_Execution,
			Formation => Initial_Formation,
			Monster_Side => Initial_Monster_Side,
			Attack => Initial_Attack,
			Vampire_Action_Set => Initial_Vampire_Action_Set,
			Servant_Knowing => Initial_Servant_Knowing,
			Daytime_Preview => Initial_Daytime_Preview,
			Doctor_Infected => Initial_Doctor_Infected,
			Hunter_Silver_Bullet => Initial_Hunter_Silver_Bullet,
			Unfortunate => Initial_Unfortunate,
			Obsolete_Teaming => Initial_Obsolete_Teaming,
			Appearance => (others => Random),
			Dummy_Role => Inhabitant,
			People => People.Empty_Vector,
			Escaped_People => People.Empty_Vector,
			Messages => Messages.Empty_Vector);
	end Create;
	
	function Count_Messages (Village : Village_Type; Day : Natural) return Message_Counts is
		Result : Message_Counts(Village.Messages.First_Index .. Village.Messages.Last_Index) := (others => (
			Speech => 0, Monologue => 0, Ghost => 0,
			Wake => 0, Encourage => 0, Encouraged => 0, Vampire_Gaze => 0, Vampire_Cancel => 0,
			Last_Action_Time => Calendar.Null_Time));
	begin
		for Position in Village.Messages.First_Index .. Village.Messages.Last_Index loop
			declare
				Current : Message renames Village.Messages.Constant_Reference(Position);
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
									Village.Escaped_People.Constant_Reference (Current.Subject).
										Id.Constant_Reference);
							begin
								if Rejoined /= No_Person
									and then Same_Id_And_Figure (
										Village.Escaped_People.Constant_Reference (Current.Subject),
										Village.People.Constant_Reference (Rejoined))
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
						when Action_Vampire_Cancel =>
							Result (Current.Subject).Vampire_Cancel := Result (Current.Subject).Vampire_Cancel + 1;
						when others =>
							null;
					end case;
				end if;
			end;
		end loop;
		return Result;
	end Count_Messages;
	
	function Night_To_Daytime (Village : Village_Type) return Ada.Calendar.Time is
	begin
		return Village.Dawn + Village.Night_Duration;
	end Night_To_Daytime;
	
	function Infection_In_First_Time (Village : Village_Type) return Ada.Calendar.Time is
	begin
		return Village.Night_To_Daytime + Village.Day_Duration / 2; -- 24h
	end Infection_In_First_Time;
	
	function Preliminary_Vote_Time (Village : Village_Type) return Ada.Calendar.Time is
	begin
		if Village.Today = 1
			and then Village.Vote = Preliminary_And_Final
			and then Village.Execution /= From_Second
		then
			return Village.Night_To_Daytime + Village.Day_Duration; -- 48h
		else
			return Village.Night_To_Daytime + Village.Day_Duration / 2; -- 24h
		end if;
	end Preliminary_Vote_Time;
	
	function Daytime_To_Vote (Village : Village_Type) return Ada.Calendar.Time is
	begin
		if Village.Today = 1
			and then Village.Vote = Preliminary_And_Final
			and then Village.Execution /= From_Second
		then
			return Village.Night_To_Daytime + Village.Day_Duration * 3 / 2; -- 72h
		else
			return Village.Night_To_Daytime + Village.Day_Duration; -- 48h
		end if;
	end Daytime_To_Vote;
	
	function Vote_To_Night (Village : Village_Type) return Ada.Calendar.Time is
	begin
		return Village.Daytime_To_Vote + Vote_Duration;
	end Vote_To_Night;
	
	function No_Commit (Village : Village_Type) return Boolean is
	begin
		for I in Village.People.First_Index .. Village.People.Last_Index loop
			if Village.People.Constant_Reference(I).Records.Constant_Reference(Village.Today).State /= Died then
				if Village.People.Constant_Reference(I).Commited then
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
			if Village.People.Constant_Reference(I).Records.Constant_Reference(Village.Today).State /= Died then
				Count := Count + 1;
				if Village.People.Constant_Reference(I).Commited then
					Commited_Count := Commited_Count + 1;
				end if;
			end if;
		end loop;
		return (Count >= Minimum_Number_Of_Persons or else Village.State /= Prologue)
			and then Commited_Count = Count;
	end Commit_Finished;
	
	procedure Join (
		Village : in out Village_Type;
		Id : in String;
		Group : in Casts.Group;
		Figure : in Casts.Person;
		Work : in Casts.Work;
		Request : in Requested_Role;
		Ignore_Request : in Boolean;
		Time : in Ada.Calendar.Time)
	is
		pragma Assert (Figure.Group = Group.Group);
	begin
		Village.Face_Group := Group.Group;
		Village.Face_Width := Group.Width;
		Village.Face_Height := Group.Height;
		Append (
			Village.People,
			Villages.Person_Type'(
				Name => Figure.Name,
				Image => Figure.Image,
				Sex => Figure.Sex,
				Group => Figure.Group,
				Work => Work.Name,
				Request => Request,
				Ignore_Request => Ignore_Request,
				Role => Inhabitant,
				Id => +Id,
				Commited => False,
				Records => To_Vector (Default_Person_Record, Length => 1)));
		Append (
			Village.Messages,
			Message'(
				Kind => Join,
				Day => Village.Today,
				Time => Time,
				Subject => Village.People.Last_Index,
				Target => People.No_Index,
				Text => Ada.Strings.Unbounded.Null_Unbounded_String));
	end Join;
	
	function Escape_Duration(Village : Village_Type) return Duration is
	begin
		case Village.Term is
			when Short => return 12 * 60 * 60 * 1.0;
			when Long => return 7 * 24 * 60 * 60 * 1.0;
		end case;
	end Escape_Duration;
	
	procedure Escape (
		Village : in out Village_Type;
		Subject : Natural;
		Time : Ada.Calendar.Time)
	is
		Escaped_Index : Natural;
	begin
		Append (Village.Escaped_People, Village.People.Constant_Reference (Subject));
		declare
			Position : People.Cursor := People.To_Cursor (Village.People, Subject);
		begin
			Delete (Village.People, Position);
		end;
		Escaped_Index := Village.Escaped_People.Last_Index;
		for I in Village.Messages.First_Index .. Village.Messages.Last_Index loop
			declare
				Kind : constant Villages.Message_Kind :=
					Village.Messages.Constant_Reference (I).Kind;
			begin
				if Kind = Escaped_Join then
					null;
				elsif Kind = Escaped_Speech then
					null;
				elsif Kind = Escape then
					null;
				elsif Kind = Speech and then Village.Messages.Constant_Reference (I).Subject = Subject then
					Village.Messages.Reference (I).Kind := Escaped_Speech;
					Village.Messages.Reference (I).Subject := Escaped_Index;
				elsif Kind = Join and then Village.Messages.Constant_Reference (I).Subject = Subject then
					Village.Messages.Reference (I).Kind := Escaped_Join;
					Village.Messages.Reference (I).Subject := Escaped_Index;
				elsif Village.Messages.Constant_Reference (I).Subject > Subject then
					declare
						Shifted_Subject : constant Integer := Village.Messages.Constant_Reference (I).Subject - 1;
					begin
						Village.Messages.Reference (I).Subject := Shifted_Subject;
					end;
				end if;
			end;
		end loop;
		Append (Village.Messages, Message'(
			Kind => Escape,
			Day => Village.Today,
			Time => Time,
			Subject => Escaped_Index,
			Target => People.No_Index,
			Text => Ada.Strings.Unbounded.Null_Unbounded_String));
	end Escape;
	
	function Vote_State (Village : Village_Type) return Vote_State_Type is
	begin
		if Village.State /= Playing then
			return Disallowed;
		else
			case Village.Execution is
				when Dummy_Killed_And_From_First | From_First =>
					null;
				when Infection_And_From_First =>
					if Village.Today = 1 and then not Village.Infected_In_First then
						return Disallowed;
					end if;
				when From_Second =>
					if Village.Today < 2 then
						return Disallowed;
					end if;
			end case;
			if Village.Time = Night then
				return Disallowed;
			elsif Village.Vote = Preliminary_And_Final and then not Preliminary_Voted (Village) then
				return Allowed_For_Preliminary;
			else
				return Allowed;
			end if;
		end if;
	end Vote_State;
	
	function Vote_Finished(Village : Village_Type) return Boolean is
	begin
		for I in Village.People.First_Index .. Village.People.Last_Index loop
			declare
				P : Person_Type renames Village.People.Constant_Reference(I);
			begin
				if P.Records.Constant_Reference(Village.Today).State /= Died
					and then not P.Commited
					and then P.Records.Constant_Reference(Village.Today).Vote < 0
				then
					return False;
				end if;
			end;
		end loop;
		return True;
	end Vote_Finished;
	
	function Voted_Count (Village : Village_Type; Day : Natural; Preliminary : Boolean) return Voted_Count_Info is
	begin
		return Result : Voted_Count_Info := (
			Last => Village.People.Last_Index,
			Max => 0,
			Counts => (Village.People.First_Index .. Village.People.Last_Index => 0))
		do
			for I in Result.Counts'Range loop
				declare
					P : Person_Type renames Village.People.Constant_Reference (I);
					V : Integer;
				begin
					if Preliminary then
						V := P.Records.Constant_Reference (Day).Provisional_Vote;
					else
						V := P.Records.Constant_Reference (Day).Vote;
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
	
	procedure Vote (
		Village : in out Village_Type;
		Subject : in Natural;
		Target : in Person_Index'Base)
	is
		Rec : Person_Record renames Village.People.Reference(Subject).Records.Reference(Village.Today);
	begin
		pragma Assert(Target < 0 or else Village.People.Constant_Reference(Target).Records.Constant_Reference(Village.Today).Candidate);
		Rec.Vote := Target;
		if not Preliminary_Voted (Village)
			and then Village.Time = Daytime -- 短期の投票延長期間は仮投票は発生させない
		then
			Rec.Provisional_Vote := Target;
		end if;
	end Vote;
	
	procedure Wake (
		Village : in out Village_Type;
		Subject : in Person_Index;
		Target : in Person_Index;
		Time : in Ada.Calendar.Time) is
	begin
		Village.People.Reference (Target).Commited := False;
		Append (Village.Messages, Message'(
			Kind => Action_Wake,
			Day => Village.Today,
			Time => Time,
			Subject => Subject,
			Target => Target,
			Text => Ada.Strings.Unbounded.Null_Unbounded_String));
	end Wake;
	
	procedure Encourage (
		Village : in out Village_Type;
		Subject : in Person_Index;
		Target : in Person_Index;
		Time : in Ada.Calendar.Time) is
	begin
		Append (Village.Messages, Message'(
			Kind => Action_Encourage,
			Day => Village.Today,
			Time => Time,
			Subject => Subject,
			Target => Target,
			Text => Ada.Strings.Unbounded.Null_Unbounded_String));
	end Encourage;
	
	function Can_Gaze (Village : Village_Type) return Boolean is
	begin
		if Village.Time = Night then
			return False; -- 夜は直接会話できるので視線を使うまでもない
		elsif Village.Today = 1 and then Village.Execution = Infection_And_From_First then
			return Village.Infected_In_First; -- 初回感染後のみ
		else
			return True; -- 2日目以降はOK
		end if;
	end Can_Gaze;
	
	procedure Vampire_Gaze (
		Village : in out Village_Type;
		Subject : in Person_Index;
		Target : in Person_Index;
		Time : in Ada.Calendar.Time)
	is
		Kind : Message_Kind;
	begin
		if Unfortunate (Village) then
			Kind := Action_Vampire_Gaze_Blocked;
		else
			Kind := Action_Vampire_Gaze;
		end if;
		Append (Village.Messages, Message'(
			Kind => Kind,
			Day => Village.Today,
			Time => Time,
			Subject => Subject,
			Target => Target,
			Text => Ada.Strings.Unbounded.Null_Unbounded_String));
	end Vampire_Gaze;
	
	procedure Vampire_Cancel (
		Village : in out Village_Type;
		Subject : in Person_Index;
		Target : in Person_Index;
		Time : in Ada.Calendar.Time) is
	begin
		Append (Village.Messages, Message'(
			Kind => Action_Vampire_Cancel,
			Day => Village.Today,
			Time => Time,
			Subject => Subject,
			Target => Target,
			Text => Ada.Strings.Unbounded.Null_Unbounded_String));
		for I in Village.People.First_Index .. Village.People.Last_Index loop
			declare
				Person : Person_Type renames Village.People.Reference (I);
			begin
				if Person.Role in Vampire_Role
					and then I /= Subject -- 自分以外
					and then Person.Records.Constant_Reference (Village.Today).Target = Target
				then
					Person.Records.Reference (Village.Today).Target := No_Person;
					Append (Village.Messages, Message'(
						Kind => Action_Vampire_Canceled,
						Day => Village.Today,
						Time => Time,
						Subject => I,
						Target => Target,
						Text => Ada.Strings.Unbounded.Null_Unbounded_String));
				end if;
			end;
		end loop;
	end Vampire_Cancel;
	
	function Infected_In_First (Village : Village_Type) return Boolean is
		pragma Assert (Village.Today = 1);
	begin
		for I in reverse Village.Messages.First_Index .. Village.Messages.Last_Index loop
			declare
				It : Message renames Village.Messages.Constant_Reference (I);
			begin
				exit when It.Day /= Village.Today;
				if It.Kind = Foreboding then
					return True;
				end if;
			end;
		end loop;
		return False;
	end Infected_In_First;
	
	function Is_Anyone_Died (Village : Village_Type; Day : Natural) return Boolean is
	begin
		for I in Village.People.First_Index .. Village.People.Last_Index loop
			if Village.People.Constant_Reference (I).Records.Constant_Reference (Day).State = Died then
				return True;
			end if;
		end loop;
		return False;
	end Is_Anyone_Died;
	
	function Find_Superman(Village : Village_Type; Role : Person_Role) return Person_Index'Base is
	begin
		for I in Village.People.First_Index .. Village.People.Last_Index loop
			if Village.People.Constant_Reference(I).Role = Role then
				return I;
			end if;
		end loop;
		return No_Person;
	end Find_Superman;
	
	function Target_Day (Village : Village_Type) return Integer is
	begin
		if Village.Time = Night
			or else (Village.Execution = Infection_And_From_First
				and then Village.Today = 1
				and then not Village.Infected_In_First)
		then
			return Village.Today - 1; -- 夜(と初日感染用)の能力は前日分を使用する
		else
			return Village.Today;
		end if;
	end Target_Day;
	
	function Astronomer_Target_Day (Village : Village_Type) return Integer is
	begin
		-- 天文家は初日感染とか気にせず日課の観測をする
		if Village.Time = Night then
			return Village.Today - 1; -- 夜の能力は前日分を使用する
		else
			return Village.Today;
		end if;
	end Astronomer_Target_Day;
	
	function Already_Used_Special (Village : Village_Type; Subject : Person_Index) return Boolean is
	begin
		for I in 1 .. Village.Target_Day - 1 loop -- 今日の設定は変えられるので昨日の分まで
			if Village.People.Constant_Reference (Subject).Records.Constant_Reference (I).Special then
				return True;
			end if;
		end loop;
		return False;
	end Already_Used_Special;
	
	function Detective_State (Village : Village_Type; Subject : Person_Index) return Ability_State is
		Subject_Person : Person_Type renames Village.People.Constant_Reference (Subject);
		pragma Assert (Subject_Person.Role = Detective);
	begin
		if Village.Daytime_Preview /= None
			and then Subject_Person.Records.Constant_Reference (Village.Today).Target /= No_Person
		then
			return Already_Used;
		elsif Village.Time /= Night and then Village.Is_Anyone_Died (Village.Today) then
			return Allowed;
		else
			return Disallowed;
		end if;
	end Detective_State;
	
	function Doctor_State (Village : Village_Type; Subject : Person_Index) return Ability_State is
		Subject_Person : Person_Type renames Village.People.Constant_Reference (Subject);
		pragma Assert (Subject_Person.Role = Doctor);
	begin
		if Village.Daytime_Preview /= None
			and then Subject_Person.Records.Constant_Reference (Village.Today).Target /= No_Person
		then
			return Already_Used;
		elsif Village.Time /= Night
			and then (Village.Today >= 2
				or else (Village.Today = 1 and then Village.Infected_In_First))
		then
			return Allowed;
		else
			return Disallowed;
		end if;
	end Doctor_State;
	
	function Superman_State (Village : Village_Type; Subject : Person_Index) return Ability_State is
		Subject_Person : Person_Type renames Village.People.Constant_Reference (Subject);
	begin
		case Subject_Person.Role is
			when Detective => return Village.Detective_State (Subject);
			when Doctor => return Village.Doctor_State (Subject);
			when others => return Allowed;
		end case;
	end Superman_State;
	
	function Silver_Bullet_State (Village : Village_Type; Subject : Person_Index) return Ability_State is
		pragma Assert (Village.People.Constant_Reference (Subject).Role = Hunter);
	begin
		if Village.Already_Used_Special (Subject) then
			return Already_Used;
		elsif Village.Execution = Infection_And_From_First
			and then Village.Today = 1
			and then not Village.Infected_In_First
		then
			return Disallowed;
		else
			return Allowed;
		end if;
	end Silver_Bullet_State;
	
	procedure Select_Target (
		Village : in out Village_Type;
		Subject : in Person_Index;
		Target : in Person_Index'Base;
		Special : in Boolean := False;
		Time : in Ada.Calendar.Time)
	is
		Target_Day : Natural := Village.Target_Day;
	begin
		case Village.People.Constant_Reference (Subject).Role is
			when Doctor =>
				if Village.Daytime_Preview /= None and then Target /= No_Person then
					declare
						Result : Message_Kind;
					begin
						if Village.People.Constant_Reference (Target).Role = Gremlin then
							-- 妖魔発見
							Result := Doctor_Found_Gremlin_Preview;
						elsif Village.People.Constant_Reference (Target).
							Records.Constant_Reference (Target_Day).State = Infected
						then
							-- 感染者発見
							if Village.Doctor_Infected = Find_Infection
								and then Village.People.Constant_Reference (Subject).
									Records.Constant_Reference (Target_Day).State = Infected
							then
								Result := Doctor_Found_Infection_Preview;
							else
								Result := Doctor_Cure_Preview;
							end if;
						else
							Result := Doctor_Failed_Preview;
						end if;
						Append (Village.Messages, Message'(
							Kind => Result,
							Day => Village.Today,
							Time => Time,
							Subject => Subject,
							Target => Target,
							Text => Ada.Strings.Unbounded.Null_Unbounded_String));
					end;
				end if;
				Village.People.Reference (Subject).Records.Reference (Target_Day).Target := Target;
			when Detective =>
				if Village.Daytime_Preview /= None and then Target /= No_Person then
					Append (Village.Messages, Message'(
						Kind => Detective_Survey_Preview,
						Day => Village.Today,
						Time => Time,
						Subject => Subject,
						Target => Target,
						Text => Village.People.Constant_Reference (Target).Records.Constant_Reference (Village.Today).Note));
				end if;
				Village.People.Reference (Subject).Records.Reference (Target_Day).Target := Target;
			when Astronomer =>
				Target_Day := Village.Astronomer_Target_Day;
				Village.People.Reference (Subject).Records.Reference (Target_Day).Target := Target;
			when others =>
				Village.People.Reference (Subject).Records.Reference (Target_Day).Target := Target;
				Village.People.Reference (Subject).Records.Reference (Target_Day).Special := Special;
		end case;
	end Select_Target;
	
	procedure Night_Talk (
		Village : in out Village_Type;
		Subject : in Natural;
		Text : in String;
		Time : in Ada.Calendar.Time) is
	begin
		if Unfortunate (Village) then
			for I in reverse Village.Messages.First_Index .. Village.Messages.Last_Index loop
				declare
					Message : Villages.Message renames Village.Messages.Constant_Reference (I);
				begin
					if Message.Day < Village.Today then
						Append (Village.Messages, Villages.Message'(
							Kind => Howling_Blocked,
							Day => Village.Today,
							Time => Time,
							Subject => No_Person,
							Target => No_Person,
							Text => Ada.Strings.Unbounded.Null_Unbounded_String));
						exit;
					end if;
					exit when Message.Kind = Howling_Blocked;
				end;
			end loop;
			-- 夜明け時に1発言はできるので記録しておく
			Village.People.Reference (Subject).Records.Reference (Village.Today - 1).Note := +Text;
		else
			Append (Village.Messages, Message'(
				Kind => Howling,
				Day => Village.Today,
				Time => Time,
				Subject => Subject,
				Target => No_Person,
				Text => +Text));
			Village.People.Reference (Subject).Records.Reference (Village.Today - 1).Note := Ada.Strings.Unbounded.Null_Unbounded_String;
		end if;
	end Night_Talk;
	
	procedure Speech (
		Village : in out Village_Type;
		Subject : in Person_Index;
		Text : in String;
		Time : in Ada.Calendar.Time)
	is
		New_Item : constant Message := (
			Kind => Speech,
			Day => Village.Today,
			Time => Time,
			Subject => Subject,
			Target => No_Person,
			Text => +Text);
	begin
		if Village.Messages.Is_Empty
		   or else not Equivalent_Messages (New_Item, Village.Messages (Village.Messages.Last))
		then
			Append (Village.Messages, New_Item);
		end if;
	end Speech;
	
	procedure Monologue (
		Village : in out Village_Type;
		Subject : in Person_Index;
		Text : in String;
		Time : in Ada.Calendar.Time)
	is
		New_Item : constant Message := (
			Kind => Monologue,
			Day => Village.Today,
			Time => Time,
			Subject => Subject,
			Target => No_Person,
			Text => +Text);
	begin
		if Village.Messages.Is_Empty
			or else not Equivalent_Messages (New_Item, Village.Messages (Village.Messages.Last))
		then
			Append (Village.Messages, New_Item);
		end if;
	end Monologue;
	
	procedure Ghost (
		Village : in out Village_Type;
		Subject : in Person_Index;
		Text : in String;
		Time : in Ada.Calendar.Time)
	is
		New_Item : constant Message := (
			Kind => Ghost,
			Day => Village.Today,
			Time => Time,
			Subject => Subject,
			Target => No_Person,
			Text => +Text);
	begin
		if Village.Messages.Is_Empty
			or else not Equivalent_Messages (New_Item, Village.Messages (Village.Messages.Last))
		then
			Append (Village.Messages, New_Item);
		end if;
	end Ghost;
	
	procedure Narration (
		Village : in out Village_Type;
		Text : in String;
		Time : in Ada.Calendar.Time)
	is
		New_Item : constant Message := (
			Kind => Narration,
			Day => Village.Today,
			Time => Time,
			Subject => No_Person,
			Target => No_Person,
			Text => +Text);
	begin
		if Village.Messages.Is_Empty
			or else not Equivalent_Messages (New_Item, Village.Messages (Village.Messages.Last))
		then
			Append (Village.Messages, New_Item);
		end if;
	end Narration;
	
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
			Process (I, Village.People.Constant_Reference (I));
		end loop;
	end Iterate_People;
	
	overriding procedure Iterate_Escaped_People (
		Village : in Village_Type;
		Process : not null access procedure (
			Index : Person_Index;
			Item : in Tabula.Villages.Person_Type'Class)) is
	begin
		for I in Village.Escaped_People.First_Index .. Village.Escaped_People.Last_Index loop
			Process (I, Village.Escaped_People.Constant_Reference (I));
		end loop;
	end Iterate_Escaped_People;
	
	function Message_Range (
		Village : Village_Type;
		Day : Natural)
		return Message_Range_Type
	is
		First : Natural := 0;
		Last : Integer := -1;
	begin
		for Position in Village.Messages.First_Index .. Village.Messages.Last_Index loop
			declare
				Current : Message renames Village.Messages.Constant_Reference (Position);
			begin
				if Current.Day = Day and then (Current.Kind = Speech or else Current.Kind = Escaped_Speech) then
					Last := Last + 1;
				end if;
			end;
		end loop;
		return (First, Last);
	end Message_Range;
	
	overriding function Recent_Only_Message_Range (
		Village : Village_Type;
		Day : Natural;
		Now : Ada.Calendar.Time)
		return Message_Range_Type
	is
		function Escaped (Village : Village_Type; Message : Villages.Message) return Boolean is
		begin
			case Message.Kind is
				when Introduction | Narration =>
					return True;
				when Escaped_Join | Escaped_Speech | Escape =>
					declare
						Rejoined : Person_Index'Base := Village.Joined (
							Village.Escaped_People.Constant_Reference (Message.Subject).
								Id.Constant_Reference);
					begin
						return Rejoined = No_Person
							or else not Same_Id_And_Figure (
								Village.Escaped_People.Constant_Reference (Message.Subject),
								Village.People.Constant_Reference (Rejoined));
					end;
				when others =>
					return False;
			end case;
		end Escaped;
		Result : Message_Range_Type := Village.Message_Range (Day);
	begin
		if Village.State = Prologue and then Day = 0 then
			declare
				Index : Natural := 0;
			begin
				while Index <= Village.Messages.Last_Index loop
					declare
						Item : Message
							renames Village.Messages.Constant_Reference (Index);
					begin
						exit when not Escaped (Village, Item)
							and then Now - Item.Time <= 180 * 24 * 60 * 60.0;
						case Item.Kind is
							when Speech | Escaped_Speech =>
								Result.First := Result.First + 1;
							when others =>
								null;
						end case;
						Index := Index + 1;
					end;
				end loop;
			end;
		end if;
		return Result;
	end Recent_Only_Message_Range;
	
	overriding procedure Iterate_Options (
		Village : in Village_Type;
		Process : not null access procedure (Item : in Root_Option_Item'Class)) is
	begin
		Process (Options.Day_Duration.Option_Item'(Village => Village'Access));
		Process (Options.Night_Duration.Option_Item'(Village => Village'Access));
		Process (Options.Vote.Option_Item'(Village => Village'Access));
		Process (Options.Execution.Option_Item'(Village => Village'Access));
		Process (Options.Formation.Option_Item'(Village => Village'Access));
		Process (Options.Monster_Side.Option_Item'(Village => Village'Access));
		Process (Options.Attack.Option_Item'(Village => Village'Access));
		Process (Options.Vampire_Action_Set.Option_Item'(Village => Village'Access));
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
				if Value'Length /= 0 and then Available (Item) then
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
				if Value'Length /= 0 and then Available (Item) then
					V.Night_Duration := Duration'Value (Value);
				end if;
			end Change;
			
		end Night_Duration;
		
		package body Vote is
			
			overriding function Available (Item : Option_Item) return Boolean is
			begin
				return True;
			end Available;
			
			overriding function Name (Item : Option_Item) return String is
			begin
				return "vote";
			end Name;
			
			overriding function Changed (Item : Option_Item) return Boolean is
			begin
				return Item.Village.Vote /= Initial_Vote;
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
					Vote_Mode'Image (Unsigned),
					Item.Village.Vote = Unsigned,
					"無記名投票で処刑を行います。",
					False);
				Process (
					Vote_Mode'Image (Preliminary_And_Final),
					Item.Village.Vote = Preliminary_And_Final,
					"仮投票と本投票で処刑を行います。",
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
				V.Vote := Vote_Mode'Value (Value);
			end Change;
			
		end Vote;
		
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
					"能力者が死亡済みの可能性があります。",
					False);
				Process (
					Execution_Mode'Image (Infection_And_From_First),
					Item.Village.Execution = Infection_And_From_First,
					"誰かが感染させられ、初日から処刑を行います。",
					False);
				Process (
					Execution_Mode'Image (From_First),
					Item.Village.Execution = From_First,
					"初日から処刑を行います。",
					False);
				Process (
					Execution_Mode'Image (From_Second),
					Item.Village.Execution = From_Second,
					"2日目から処刑を行います。",
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
				-- 記録用
				case V.Execution is
					when Dummy_Killed_And_From_First | From_First | From_Second =>
						V.Obsolete_Teaming := Shuffling;
					when Infection_And_From_First =>
						V.Obsolete_Teaming := Liner_2;
				end case;
			end Change;
			
		end Execution;
		
		package body Formation is
			
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
				return Item.Village.Formation /= Initial_Formation;
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
					Formation_Mode'Image (Public),
					Item.Village.Formation = Public,
					"村側能力者の構成を公開します。",
					False);
				Process (
					Formation_Mode'Image (Hidden),
					Item.Village.Formation = Hidden,
					"村側能力者の構成はわかりません。",
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
				V.Formation := Formation_Mode'Value (Value);
			end Change;
			
		end Formation;
		
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
					Attack_Mode'Image (Nocturnal_Chain_Infecting),
					Item.Village.Attack = Nocturnal_Chain_Infecting,
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
		
		package body Vampire_Action_Set is
			
			overriding function Available (Item : Option_Item) return Boolean is
			begin
				return True;
			end Available;
			
			overriding function Name (Item : Option_Item) return String is
			begin
				return "vampire-action-set";
			end Name;
			
			overriding function Changed (Item : Option_Item) return Boolean is
			begin
				return Item.Village.Vampire_Action_Set /= Initial_Vampire_Action_Set;
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
					Vampire_Action_Set_Mode'Image (None),
					Item.Village.Vampire_Action_Set = None,
					"吸血鬼には特別なアクションはありません。",
					Unrecommended => True);
				Process (
					Vampire_Action_Set_Mode'Image (Gaze),
					Item.Village.Vampire_Action_Set = Gaze,
					"視線「こっそり見つめる」が使えます。",
					False);
				Process (
					Vampire_Action_Set_Mode'Image (Gaze_And_Cancel),
					Item.Village.Vampire_Action_Set = Gaze_And_Cancel,
					"視線と襲撃取り消し「襲うのをやめさせる」が使えます。",
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
				V.Vampire_Action_Set := Vampire_Action_Set_Mode'Value (Value);
			end Change;
			
		end Vampire_Action_Set;
		
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
