-- The Village of Vampire by YT, このソースコードはNYSLです
package body Vampire.Villages.Teaming is
	use type Ada.Containers.Count_Type;
	use type Casts.Person_Sex;
	
	function Possibilities (
		People_Count : Ada.Containers.Count_Type;
		Male_And_Female : Boolean;
		Execution : Execution_Mode;
		Formation : Formation_Mode;
		Unfortunate : Unfortunate_Mode;
		Monster_Side : Monster_Side_Mode)
		return Role_Set_Array
	is
		Result : Role_Set_Array (1 .. 2 ** (Role_Set'Length - 2));
		Last : Natural := 0;
		
		subtype Village_Side_Superman_Count_Type is Natural range 0 .. 7; -- 天猟探医数恋恋
		subtype Vampire_Count_Type is Natural range 0 .. 4; -- KQJ + 一時的に使徒を計算に含める分
		subtype Servant_Count_Type is Natural range 0 .. 1;
		subtype Gremlin_Count_Type is Natural range 0 .. 1;
		
		procedure Add (Set : in Role_Set) is
		begin
			Last := Last + 1;
			Result (Last) := Set;
		end Add;
		
		procedure Process_Inhabitants (
			Set : in Role_Set;
			Village_Side_Superman_Count : in Village_Side_Superman_Count_Type;
			Total_Village_Side_Superman_Count : in Village_Side_Superman_Count_Type)
		is
			Set_2 : Role_Set := Set;
			Count : Ada.Containers.Count_Type := 0;
		begin
			if Village_Side_Superman_Count = 0
				or else (
					Village_Side_Superman_Count = 1
					and then Total_Village_Side_Superman_Count >= 3
					and then Unfortunate /= None)
			then
				if Village_Side_Superman_Count = 1 then
					Set_2 (Unfortunate_Inhabitant) := 1;
				end if;
				for I in Set_2'Range loop
					Count := Count + Ada.Containers.Count_Type (Set_2 (I));
				end loop;
				Set_2 (Inhabitant) := Natural (
					Ada.Containers.Count_Type'Max (0, People_Count - Count));
				Add (Set_2);
			end if;
		end Process_Inhabitants;
		
		procedure Process_Lovers (
			Set : in Role_Set;
			Village_Side_Superman_Count : in Village_Side_Superman_Count_Type;
			Total_Village_Side_Superman_Count : in Village_Side_Superman_Count_Type) is
		begin
			Process_Inhabitants (Set, Village_Side_Superman_Count, Total_Village_Side_Superman_Count);
			if Male_And_Female then
				if Village_Side_Superman_Count >= 1 and then Total_Village_Side_Superman_Count >= 3 then
					declare
						Set_2 : Role_Set := Set;
					begin
						Set_2 (Lover) := 1;
						Set_2 (Loved_Inhabitant) := 1;
						Process_Inhabitants (Set_2, Village_Side_Superman_Count - 1, Total_Village_Side_Superman_Count);
					end;
				end if;
				if Village_Side_Superman_Count >= 2 and then Total_Village_Side_Superman_Count >= 4 then
					declare
						Set_2 : Role_Set := Set;
					begin
						Set_2 (Sweetheart_M) := 1;
						Set_2 (Sweetheart_F) := 1;
						Process_Inhabitants (Set_2, Village_Side_Superman_Count - 2, Total_Village_Side_Superman_Count);
					end;
				end if;
			end if;
		end Process_Lovers;
		
		procedure Process_Detective (
			Set : in Role_Set;
			Village_Side_Superman_Count : in Village_Side_Superman_Count_Type;
			Total_Village_Side_Superman_Count : in Village_Side_Superman_Count_Type)
		is
			Set_2 : Role_Set := Set;
		begin
			Process_Lovers (Set, Village_Side_Superman_Count, Total_Village_Side_Superman_Count);
			if Village_Side_Superman_Count > 0 then
				Set_2 (Detective) := 1;
				Process_Lovers (Set_2, Village_Side_Superman_Count - 1, Total_Village_Side_Superman_Count);
			end if;
		end Process_Detective;
		
		procedure Process_Doctor (
			Set : in Role_Set;
			Village_Side_Superman_Count : in Village_Side_Superman_Count_Type;
			Total_Village_Side_Superman_Count : in Village_Side_Superman_Count_Type)
		is
			Set_2 : Role_Set := Set;
		begin
			Process_Detective (Set, Village_Side_Superman_Count, Total_Village_Side_Superman_Count);
			if Village_Side_Superman_Count > 0 then
				Set_2 (Doctor) := 1;
				Process_Detective (Set_2, Village_Side_Superman_Count - 1, Total_Village_Side_Superman_Count);
			end if;
		end Process_Doctor;
		
		procedure Process_Hunter (
			Set : in Role_Set;
			Village_Side_Superman_Count : in Village_Side_Superman_Count_Type;
			Total_Village_Side_Superman_Count : in Village_Side_Superman_Count_Type)
		is
			Set_2 : Role_Set := Set;
		begin
			if Village_Side_Superman_Count = 0
				or else (
					Set (Astronomer) >= 1
					and then (Set (Gremlin) = 0 or else Monster_Side /= Gremlin))
			then
				Process_Doctor (Set, Village_Side_Superman_Count, Total_Village_Side_Superman_Count);
			end if;
			if Village_Side_Superman_Count > 0
				and then (
					Set (Astronomer) = 0
					or else (Set (Gremlin) > 0 and then Monster_Side = Gremlin)
					or else Set (Vampire_K) + Set (Vampire_Q) + Set (Vampire_J) + Set (Servant) + Set (Gremlin) >= 3)
			then
				Set_2 (Hunter) := 1;
				Process_Doctor (Set_2, Village_Side_Superman_Count - 1, Total_Village_Side_Superman_Count);
			end if;
		end Process_Hunter;
		
		procedure Process_Astronomer (
			Set : in Role_Set;
			Village_Side_Superman_Count : in Village_Side_Superman_Count_Type;
			Total_Village_Side_Superman_Count : in Village_Side_Superman_Count_Type)
		is
			Set_2 : Role_Set := Set;
		begin
			if Village_Side_Superman_Count = 0
				or else Formation /= Hidden
			then
				Process_Hunter (Set, Village_Side_Superman_Count, Total_Village_Side_Superman_Count);
			end if;
			if Village_Side_Superman_Count > 0 then
				Set_2 (Astronomer) := 1;
				Process_Hunter (Set_2, Village_Side_Superman_Count - 1, Total_Village_Side_Superman_Count);
			end if;
		end Process_Astronomer;
		
		procedure Process_Vampires (
			Set : in Role_Set;
			Village_Side_Superman_Count : in Village_Side_Superman_Count_Type;
			Vampire_Count : in Vampire_Count_Type)
		is
			Set_2 : Role_Set := Set;
		begin
			if Vampire_Count >= 1 then
				Set_2 (Vampire_K) := 1;
				if Vampire_Count >= 2 then
					Set_2 (Vampire_Q) := 1;
					if Vampire_Count >= 3 then
						Set_2 (Vampire_J) := 1;
					end if;
				end if;
			end if;
			Process_Astronomer (Set_2, Village_Side_Superman_Count, Village_Side_Superman_Count);
		end Process_Vampires;
		
		procedure Process_Servant (
			Set : in Role_Set;
			Village_Side_Superman_Count : in Village_Side_Superman_Count_Type;
			Vampire_Count : in Vampire_Count_Type;
			Servant_Count : in Servant_Count_Type)
		is
			Set_2 : Role_Set := Set;
			Vampire_Count_2 : Vampire_Count_Type;
		begin
			if Monster_Side = Shuffling then
				Vampire_Count_2 := Vampire_Count + Servant_Count;
				Process_Vampires (
					Set_2,
					Village_Side_Superman_Count,
					Vampire_Count_2);
				if Vampire_Count_2 >= 2 then
					Vampire_Count_2 := Vampire_Count_2 - 1;
					Set_2 (Servant) := 1;
					Process_Vampires (
						Set_2,
						Village_Side_Superman_Count,
						Vampire_Count_2);
				end if;
			else
				Set_2 (Servant) := Servant_Count;
				Process_Vampires (
					Set_2,
					Village_Side_Superman_Count,
					Vampire_Count);
			end if;
		end Process_Servant;
		
		procedure Process_Gremlin (
			Set : in Role_Set;
			Village_Side_Superman_Count : in Village_Side_Superman_Count_Type;
			Vampire_Count : in Vampire_Count_Type;
			Servant_Count : in Servant_Count_Type;
			Gremlin_Count : in Gremlin_Count_Type)
		is
			Set_2 : Role_Set := Set;
		begin
			Set_2 (Gremlin) := Gremlin_Count;
			Process_Servant (
				Set_2,
				Village_Side_Superman_Count,
				Vampire_Count,
				Servant_Count);
		end Process_Gremlin;
		
		People_Count_2 : Ada.Containers.Count_Type := People_Count;
		
		Village_Side_Superman_Count : Village_Side_Superman_Count_Type;
		Vampire_Count : Vampire_Count_Type;
		Servant_Count : Servant_Count_Type := 0;
		Gremlin_Count : Gremlin_Count_Type := 0;
	begin
		if Execution = From_Second then
			People_Count_2 := People_Count_2 - 1;
		end if;
		case Execution is
			when Infection_And_From_First =>
				if People_Count_2 >= 15 then
					Village_Side_Superman_Count := 4;
				elsif People_Count_2 >= 11 then
					Village_Side_Superman_Count := 3;
				elsif People_Count_2 >= 8 then
					Village_Side_Superman_Count := 2;
				else
					Village_Side_Superman_Count := 1;
				end if;
				if People_Count_2 >= 14 then
					Vampire_Count := 3;
					Servant_Count := 1;
				elsif People_Count_2 >= 12 then
					Vampire_Count := 3;
				elsif People_Count_2 >= 10 then
					Vampire_Count := 2;
					Servant_Count := 1;
				elsif People_Count_2 >= 8 then
					Vampire_Count := 2;
				elsif People_Count_2 >= 6 then
					Vampire_Count := 1;
					Servant_Count := 1;
				else
					Vampire_Count := 1;
				end if;
				if People_Count_2 >= 16 then
					Gremlin_Count := 1;
				end if;
			when Dummy_Killed_And_From_First | From_First | From_Second =>
				if People_Count_2 >= 15 then
					Village_Side_Superman_Count := 5;
				elsif People_Count_2 >= 13 then
					Village_Side_Superman_Count := 4;
				elsif People_Count_2 >= 10 then
					Village_Side_Superman_Count := 3;
				elsif People_Count_2 >= 8 then
					Village_Side_Superman_Count := 2;
				else
					Village_Side_Superman_Count := 1;
				end if;
				if People_Count_2 >= 14 then
					Vampire_Count := 3;
					Servant_Count := 1;
				elsif People_Count_2 = 13 then
					Vampire_Count := 3;
				elsif People_Count_2 >= 9 then
					Vampire_Count := 2;
					Servant_Count := 1;
				elsif People_Count_2 = 8 then
					Vampire_Count := 2;
				elsif People_Count_2 = 7 then
					Vampire_Count := 1;
					Servant_Count := 1;
				else
					Vampire_Count := 1;
				end if;
				if People_Count_2 >= 16 then
					Gremlin_Count := 1;
				end if;
		end case;
		-- 編成隠し時能力者+1
		if Formation = Hidden then
			Village_Side_Superman_Count := Village_Side_Superman_Count + 1;
		end if;
		-- カップル作成不可能の場合は能力者の種類が足りなくなる
		if not Male_And_Female then
			if Unfortunate = None and then Village_Side_Superman_Count >= 5 then
				Village_Side_Superman_Count := 4; -- 天猟探医
			elsif Village_Side_Superman_Count >= 6 then
				Village_Side_Superman_Count := 5; -- 天猟探医奇
			end if;
		end if;
		-- 使徒妖魔交換
		if Monster_Side = Gremlin then
			declare
				T : constant Natural := Servant_Count;
			begin
				Servant_Count := Gremlin_Count;
				Gremlin_Count := T;
			end;
		end if;
		-- 組み合わせ探索
		declare
			Zero_Set : constant Role_Set := (others => 0);
		begin
			Process_Gremlin (
				Zero_Set,
				Village_Side_Superman_Count,
				Vampire_Count,
				Servant_Count,
				Gremlin_Count);
		end;
		return Result (1 .. Last);
	end Possibilities;
	
	function Select_Set (
		Sets : Role_Set_Array;
		Appearance : Role_Appearances;
		Generator : not null access Ada.Numerics.MT19937.Generator)
		return Role_Set
	is
		subtype T is Positive range Sets'Range;
		package Random is new Ada.Numerics.MT19937.Discrete_Random (T);
		Index : T;
	begin
		Index := Random.Random (Generator);
		-- 片想いと数奇な運命の村人の出現率を少し下げる
		if Sets (Index)(Lover) > 0
			or else Sets (Index)(Unfortunate_Inhabitant) > 0
		then
			Index := Random.Random (Generator);
		end if;
		-- 天文家無しの出現率を少し下げる
		if Sets (Index)(Astronomer) = 0 then
			Index := Random.Random (Generator);
		end if;
		return Sets (Index);
	end Select_Set;
	
	procedure Shuffle (
		People : in out Villages.People.Vector;
		Victim : access Villages.Person_Role;
		Set : Role_Set;
		Generator : not null access Ada.Numerics.MT19937.Generator)
	is
		subtype People_Index is Integer range People.First_Index .. People.Last_Index;
		package People_Random is new Ada.Numerics.MT19937.Discrete_Random(People_Index);
		
		type Role_Set is array (Person_Role) of Boolean;
		pragma Pack(Role_Set);
		
		function Request_To_Role_Set (Request : Requested_Role) return Role_Set is
		begin
			case Request is
				when Random =>
					return Role_Set'(others => True);
				when Rest =>
					pragma Assert(False);
					return Role_Set'(others => False);
				when Inhabitant =>
					return Role_Set'(Inhabitant | Loved_Inhabitant | Unfortunate_Inhabitant => True, others => False);
				when Detective =>
					return Role_Set'(Detective => True, others => False);
				when Astronomer =>
					return Role_Set'(Astronomer => True, others => False);
				when Doctor =>
					return Role_Set'(Doctor => True, others => False);
				when Hunter =>
					return Role_Set'(Hunter => True, others => False);
				when Sweetheart =>
					return Role_Set'(Sweetheart_M | Sweetheart_F | Lover => True, others => False);
				when Servant =>
					return Role_Set'(Servant => True, others => False);
				when Vampire =>
					return Role_Set'(Vampire_Role => True, others => False);
				when Village_Side =>
					return Role_Set'(Vampire_Role | Servant => False, Gremlin => False, others => True);
				when Vampire_Side =>
					return Role_Set'(Vampire_Role | Servant => True, Gremlin => False, others => False);
				when Gremlin =>
					return Role_Set'(Gremlin => True, others => False);
			end case;
		end Request_To_Role_Set;
		
		type Request_Matrix is array (People_Index) of Role_Set;
		
		function Get_Request_Matrix return Request_Matrix is
			function Get_Rest_Role_Set return Role_Set is
				Result : Role_Set := (others => True);
			begin
				for I in People_Index loop
					declare
						Person : Person_Type renames People.Constant_Reference(I).Element.all;
					begin
						if not Person.Ignore_Request then
							declare
								R : constant Requested_Role := Person.Request;
							begin
								if R /= Rest then
									Result := Result and not Request_To_Role_Set(R);
								end if;
							end;
						end if;
					end;
				end loop;
				return Result;
			end Get_Rest_Role_Set;
			Rest_Roles : constant Role_Set := Get_Rest_Role_Set;
			Result : Request_Matrix;
		begin
			for I in People_Index loop
				declare
					Person : Person_Type renames People.Constant_Reference(I).Element.all;
				begin
					if Person.Ignore_Request then
						Result(I) := (Inhabitant => True, others => False); -- 強制村人
					else
						declare
							R : constant Requested_Role := Person.Request;
						begin
							if R /= Rest then
								Result(I) := Request_To_Role_Set(R);
							else
								Result(I) := Rest_Roles;
							end if;
						end;
					end if;
				end;
			end loop;
			return Result;
		end Get_Request_Matrix;
		
		Request : constant Request_Matrix := Get_Request_Matrix;
		
		type Assignment is array(People_Index) of Person_Role;
		
		function Random_Assignment return Assignment is
			Result : Assignment := (others => Inhabitant);
		begin
			for I in Person_Role loop
				for J in 1 .. Set (I) loop
					Selecting : loop
						declare
							Who : constant People_Index := People_Random.Random(Generator);
						begin
							if Result(Who) = Inhabitant then
								Result(Who) := I;
								exit Selecting;
							end if;
						end;
					end loop Selecting;
				end loop;
			end loop;
			return Result;
		end Random_Assignment;
		
		function Evaluate (Candidacy : Assignment) return Integer is
			Bad : constant Integer := -1;
			Result : Integer := 0;
			Lover_Sex : Casts.Person_Sex := Casts.Male;
			Loved_Sex : Casts.Person_Sex := Casts.Female;
		begin
			for I in People_Index loop
				declare
					S : constant Casts.Person_Sex := People.Constant_Reference(I).Element.Sex;
				begin
					if (S = Casts.Male and Candidacy (I) = Sweetheart_F)
						or else (S = Casts.Female and Candidacy (I) = Sweetheart_M)
					then
						return Bad;
					elsif Candidacy(I) = Lover then
						Lover_Sex := S;
					elsif Candidacy(I) = Loved_Inhabitant then
						Loved_Sex := S;
					end if;
				end;
				if Request (I)(Candidacy (I)) then
					declare
						Person : Person_Type renames People.Constant_Reference(I).Element.all;
					begin
						if Person.Ignore_Request then
							Result := Result + 20;
						else
							case Person.Request is
								when Random | Village_Side =>
									Result := Result + 1;
								when Rest =>
									Result := Result + 10;
								when others =>
									Result := Result + 2;
							end case;
						end if;
					end;
				end if;
			end loop;
			if Loved_Sex = Lover_Sex then
				return Bad;
			end if;
			return Result;
		end Evaluate;
		
		type Gene is record
			Assignment : Shuffle.Assignment;
			Rating : Integer;
		end record;
		
		function Random_Valid_Gene return Gene is
			Result : Gene;
		begin
			loop
				Result.Assignment := Random_Assignment;
				Result.Rating := Evaluate(Result.Assignment);
				exit when Result.Rating >= 0;
			end loop;
			return Result;
		end Random_Valid_Gene;
		
		Current : Gene := Random_Valid_Gene;
	begin
		for I in 1 .. 1024 loop
			declare
				Candidacy : constant Gene := Random_Valid_Gene;
			begin
				if Candidacy.Rating > Current.Rating then
					Current := Candidacy;
				end if;
			end;
		end loop;
		-- 初日犠牲者
		if Victim /= null then
			declare
				Changing : constant People_Index := People_Random.Random(Generator);
			begin
				case Current.Assignment(Changing) is
					when Vampire_Role | Gremlin | Sweetheart_M | Sweetheart_F | Loved_Inhabitant =>
						Victim.all := Inhabitant;
					when others =>
						Victim.all := Current.Assignment(Changing);
						Current.Assignment(Changing) := Inhabitant;
				end case;
			end;
		end if;
		-- 設定
		for I in People_Index loop
			People.Reference (I).Element.Role := Current.Assignment(I);
		end loop;
	end Shuffle;
	
end Vampire.Villages.Teaming;
