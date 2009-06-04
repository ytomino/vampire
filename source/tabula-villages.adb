-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Containers.Generic_Array_Sort;
package body Tabula.Villages is
	
	overriding procedure Adjust(Object : in out Person_Type) is
	begin
		if Object.Records /= null then
			Object.Records := new Person_Record_Array'(Object.Records.all);
		end if;
	end Adjust;
	
	overriding procedure Finalize(Object : in out Person_Type) is
	begin
		Free(Object.Records);
	end Finalize;

	overriding procedure Finalize(Object : in out Village_Type) is
	begin
		Free(Object.People);
		Free(Object.Escaped_People);
	end Finalize;
	
	function Count_Messages(Village : Village_Type; Day : Natural) return Message_Counts is
		use Messages;
	begin
		if Village.People = null then
			return (0 .. -1 => <>);
		else
			declare
				Position : Messages.Cursor := First(Village.Messages);
				Result : Message_Counts(Village.People'Range) := (others => (
					Speech => 0, Monologue => 0, Ghost => 0, Wake => 0, Encourage => 0, Encouraged => 0, Vampire_Gaze => 0, 
					Last_Action_Time => Calendar.Null_Time));
			begin
				while Has_Element(Position) loop
					declare
						Current : Message renames Element(Position);
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
										Rejoined_Index : constant Integer := Rejoined(Village, Current.Subject);
									begin
										if Rejoined_Index >= 0 then
											Result(Rejoined_Index).Speech := Result(Rejoined_Index).Speech + 1;
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
					Next(Position);
				end loop;
				return Result;
			end;
		end if;
	end Count_Messages;
	
	function Count_Speech(Village : Village_Type; Day : Natural) return Natural is
		use Messages;
		Position : Messages.Cursor := First(Village.Messages);
		Result : Natural := 0;
	begin
		while Has_Element(Position) loop
			declare
				Current : Message renames Element(Position);
			begin
				if Current.Day = Day and then (Current.Kind = Speech or else Current.Kind = Escaped_Speech) then
					Result := Result + 1;
				end if;
			end;
			Next(Position);
		end loop;
		return Result;
	end Count_Speech;
	
	function Last_Joined_Time(Village : Village_Type) return Ada.Calendar.Time is
		use Messages;
		Result : Ada.Calendar.Time;
		Position : Messages.Cursor := First(Village.Messages);
	begin
		while Has_Element(Position) loop
			if Element(Position).Kind = Join then
				Result := Element(Position).Time;
			end if;
			Next(Position);
		end loop;
		return Result;
	end Last_Joined_Time;
	
	function Joined(Village : Village_Type; User_Id : String) return Integer is
		use type Ada.Strings.Unbounded.Unbounded_String;
	begin
		if Village.People /= null then
			for Position in Village.People'Range loop
				if Village.People(Position).Id = User_Id then
					return Position;
				end if;
			end loop;
		end if;
		return -1;
	end Joined;
	
	function Rejoined(Village : Village_Type; Escaped_Subject : Natural) return Integer is
		use type Ada.Strings.Unbounded.Unbounded_String;
		E : Person_Type renames Village.Escaped_People(Escaped_Subject);
		I : constant Integer := Joined(Village, Ada.Strings.Unbounded.To_String(E.Id));
	begin
		if I < 0 then
			return -1;
		else
			declare
				R : Person_Type renames Village.People(I);
			begin
				if R.Id = E.Id and then R.Image = E.Image then
					return I;
				else
					return -1;
				end if;
			end;
		end if;
	end Rejoined;
	
	function Commit_Finished(Village : Village_Type) return Boolean is
		Count : Integer := 0;
		Commited_Count : Integer := 0;
	begin
		if Village.People /= null then
			for I in Village.People'Range loop
				if Village.People(I).Records(Village.Today).State /= Villages.Died then
					Count := Count + 1;
					if Village.People(I).Commited then
						Commited_Count := Commited_Count + 1;
					end if;
				end if;
			end loop;
		end if;
		return (Count >= Minimum_Number_Of_Persons or else Village.State /= Villages.Prologue)
			and then Commited_Count = Count;
	end Commit_Finished;
	
	function Provisional_Voted(Village : Village_Type) return Boolean is
	begin
		for I in Village.Messages.First_Index .. Village.Messages.Last_Index loop
			declare
				It : Message renames Village.Messages.Element(I);
			begin
				if It.Day = Village.Today and then It.Kind = Provisional_Vote then
					return True;
				end if;
			end;
		end loop;
		return False;
	end Provisional_Voted;
	
	function Vote_Finished(Village : Village_Type) return Boolean is
	begin
		for I in Village.People'Range loop
			declare
				P : Person_Type renames Village.People(I);
			begin
				if P.Records(Village.Today).State /= Died
					and then not P.Commited
					and then P.Records(Village.Today).Vote < 0
				then
					return False;
				end if;
			end;
		end loop;
		return True;
	end Vote_Finished;
	
	function Find_Superman(Village : Village_Type; Role : Person_Role) return Integer is
	begin
		for I in Village.People'Range loop
			if Village.People(I).Role = Role then
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
				if Village.People(The_Unfortunate_Inhabitant).Records(I).State /= Normal then
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
	
	procedure Escape(Village : in out Villages.Village_Type; Subject : Natural; Time : Ada.Calendar.Time) is
		use Person_Arrays;
		use Messages;
		Escaped_Index : Natural;
	begin
		Append(Village.Escaped_People, Village.People(Subject));
		Delete(Village.People, Subject);
		Escaped_Index := Village.Escaped_People'Last;
		for I in Village.Messages.First_Index .. Village.Messages.Last_Index loop
			declare
				Kind : Villages.Message_Kind renames Element(Village.Messages, I).Kind;
			begin
				if Kind = Villages.Escaped_Join then
					null;
				elsif Kind = Villages.Escaped_Speech then
					null;
				elsif Kind = Villages.Escape then
					null;
				elsif Kind = Villages.Speech and then Element(Village.Messages, I).Subject = Subject then
					declare
						procedure Update(Message : in out Villages.Message) is
						begin
							Message.Subject := Escaped_Index;
							Message.Kind := Villages.Escaped_Speech;
						end Update;
					begin
						Update_Element(Village.Messages, I, Update'Access);
					end;
				elsif Kind = Villages.Join and then Element(Village.Messages, I).Subject = Subject then
					declare
						procedure Update(Message : in out Villages.Message) is
						begin
							Message.Subject := Escaped_Index;
							Message.Kind := Villages.Escaped_Join;
						end Update;
					begin
						Update_Element(Village.Messages, I, Update'Access);
					end;
				elsif Element(Village.Messages, I).Subject > Subject then
					declare
						procedure Update(Message : in out Villages.Message) is
						begin
							Message.Subject := Message.Subject - 1;
						end Update;
					begin
						Update_Element(Village.Messages, I, Update'Access);
					end;
				end if;
			end;
		end loop;
		Append(Village.Messages, Message'(
			Kind => Escape,
			Day => Village.Today,
			Time => Time,
			Subject => Escaped_Index,
			Target => -1,
			Text => Ada.Strings.Unbounded.Null_Unbounded_String));
	end Escape;
	
	procedure Vote(Village : in out Village_Type; Player : Natural; Target : Integer; Apply: Boolean; Time : Ada.Calendar.Time) is
		Rec : Person_Record renames Village.People(Player).Records(Village.Today);
	begin
		pragma Assert(Target < 0 or else Village.People(Target).Records(Village.Today).Candidate);
		Rec.Vote := Target;
		if not Provisional_Voted(Village)
			and then Village.Time = Daytime -- 短期の投票延長期間は仮投票は発生させない
		then
			Rec.Provisional_Vote := Target;
			Rec.Applied := Apply;
			if Apply then
				-- 開示申請の集計
				declare
					use Messages;
					type Voted_Array is array (Natural range <>) of Natural;
					procedure Sort is new Ada.Containers.Generic_Array_Sort(Natural, Natural, Voted_Array);
					Lives : array (Village.People'Range) of Boolean := (others => False);
					Valid_Votes : Natural := 0;
					Ayes : Natural := 0;
					Voted, Sort_Voted : Voted_Array(Village.People'Range) := (others => 0);
					Candidates : Natural := 0;
					Max : Natural := 0;
					Limit : Natural := 0;
				begin
					-- 前日からずっと無発言の人は集計から除く
					for I in Village.Messages.First_Index .. Village.Messages.Last_Index loop
						declare
							It : Message renames Village.Messages.Element(I);
						begin
							if It.Day in Village.Today - 1 .. Village.Today and then It.Kind = Speech then
								Lives(It.Subject) := True;
							end if;
						end;
					end loop;
					for I in Village.People'Range loop
						if not Village.People(I).Commited then
							Lives(I) := True;
						end if;
					end loop;
					-- 集計
					for I in Village.People'Range loop
						if Lives(I) then
							Valid_Votes := Valid_Votes + 1;
							if Village.People(I).Records(Village.Today).Applied then
								Ayes := Ayes + 1;
							end if;
							declare
								Target : Integer := Village.People(I).Records(Village.Today).Provisional_Vote;
							begin
								if Target in Village.People'Range then
									if Voted(Target) = 0 then
										Candidates := Candidates + 1;
									end if;
									Voted(Target) := Voted(Target) + 1;
									if Voted(Target) > Max then
										Max := Voted(Target);
									end if;
								end if;
							end;
						end if;
					end loop;
					-- 過半数が開票に賛成して、候補が2名以上いる場合に適用
					if Ayes * 2 > Valid_Votes and then Candidates >= 2 then
						-- 同率2位までを候補とする
						Sort_Voted := Voted;
						Sort(Sort_Voted);
						Limit := Sort_Voted(Sort_Voted'Last - 1);
						for I in Village.People'Range loop
							Village.People(I).Records(Village.Today).Candidate := Voted(I) >= Limit;
						end loop;
						-- 選ばれた候補以外に投票していた人は棄権に戻す
						for I in Village.People'Range loop
							declare
								V : Integer renames Village.People(I).Records(Village.Today).Vote;
							begin
								if V >= 0 and then not Village.People(V).Records(Village.Today).Candidate then
									V := -1;
								end if;
							end;
						end loop;
						Append(Village.Messages, Message'(
							Kind => Provisional_Vote,
							Day => Village.Today,
							Time => Time,
							Subject => -1,
							Target => -1,
							Text => Ada.Strings.Unbounded.Null_Unbounded_String));
					end if;
				end;
			end if;
		end if;
	end Vote;
	
	function Already_Joined_Another_Sex(Village : Village_Type; User_Id : String; Sex : Sex_Kind) return Boolean is
		use type Ada.Strings.Unbounded.Unbounded_String;
	begin
		if Village.Escaped_People /= null then
			Search_Pre : for I in reverse Village.Escaped_People'Range loop
				declare
					P : Person_Type renames Village.Escaped_People(I);
				begin
					if P.Id = User_Id then
						if P.Sex /= Sex then
							return True;
						else
							exit Search_Pre;
						end if;
					end if;
				end;
			end loop Search_Pre;
		end if;
		return False;
	end Already_Joined_Another_Sex;
	
	function Escape_Duration(Village : Village_Type) return Duration is
	begin
		if Village.Day_Duration < 24 * 60 * 60.0 then
			return 12 * 60 * 60 * 1.0;
		else
			return 7 * 24 * 60 * 60 * 1.0;
		end if;
	end Escape_Duration;

end Tabula.Villages;
