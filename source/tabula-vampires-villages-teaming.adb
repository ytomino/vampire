-- The Village of Vampire by YT, このソースコードはNYSLです
package body Tabula.Vampires.Villages.Teaming is
	use type Ada.Containers.Count_Type;
	
	function Possibilities (
		People_Count : Ada.Containers.Count_Type;
		Male_And_Female : Boolean;
		Execution : Execution_Mode;
		Teaming : Teaming_Mode;
		Monster_Side : Monster_Side_Mode;
		Appearance : Role_Appearances) return Role_Set_Array
	is
		Result : Role_Set_Array (1 .. 2 ** (Role_Set'Length - 2));
		Last : Natural := 0;
		
		subtype Village_Side_Superman_Count_Type is Natural range 0 .. 6;
		subtype Vampire_Count_Type is Natural range 0 .. 4;
		subtype Servant_Count_Type is Natural range 0 .. 1;
		subtype Gremlin_Count_Type is Natural range 0 .. 1;
		
		People_Count_2 : Ada.Containers.Count_Type := People_Count;
		
		procedure Add (Set : in Role_Set) is
		begin
			Last := Last + 1;
			Result (Last) := Set;
		end Add;
		
		procedure Process_Inhabitants (
			Set : in Role_Set;
			Village_Side_Superman_Count : in Village_Side_Superman_Count_Type)
		is
			Set_2 : Role_Set := Set;
			Count : Ada.Containers.Count_Type := 0;
		begin
			if Village_Side_Superman_Count = 0 or else
				(Village_Side_Superman_Count = 1 and then People_Count_2 >= 8)
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
			Village_Side_Superman_Count : in Village_Side_Superman_Count_Type) is
		begin
			Process_Inhabitants (Set, Village_Side_Superman_Count);
			if Male_And_Female then
				if Village_Side_Superman_Count >= 1 and then People_Count_2 >= 8 then
					declare
						Set_2 : Role_Set := Set;
					begin
						Set_2 (Lover) := 1;
						Set_2 (Loved_Inhabitant) := 1;
						Process_Inhabitants (Set_2, Village_Side_Superman_Count - 1);
					end;
				end if;
				if Village_Side_Superman_Count >= 2 and then People_Count_2 >= 13 then
					declare
						Set_2 : Role_Set := Set;
					begin
						Set_2 (Sweetheart_M) := 1;
						Set_2 (Sweetheart_F) := 1;
						Process_Inhabitants (Set_2, Village_Side_Superman_Count - 2);
					end;
				end if;
			end if;
		end Process_Lovers;
		
		procedure Process_Detective (
			Set : in Role_Set;
			Village_Side_Superman_Count : in Village_Side_Superman_Count_Type)
		is
			Set_2 : Role_Set := Set;
		begin
			Process_Lovers (Set, Village_Side_Superman_Count);
			if Village_Side_Superman_Count > 0 then
				Set_2 (Detective) := 1;
				Process_Lovers (Set_2, Village_Side_Superman_Count - 1);
			end if;
		end Process_Detective;
		
		procedure Process_Doctor (
			Set : in Role_Set;
			Village_Side_Superman_Count : in Village_Side_Superman_Count_Type)
		is
			Set_2 : Role_Set := Set;
		begin
			Process_Detective (Set, Village_Side_Superman_Count);
			if Village_Side_Superman_Count > 0 then
				Set_2 (Doctor) := 1;
				Process_Detective (Set_2, Village_Side_Superman_Count - 1);
			end if;
		end Process_Doctor;
		
		procedure Process_Hunter (
			Set : in Role_Set;
			Village_Side_Superman_Count : in Village_Side_Superman_Count_Type)
		is
			Set_2 : Role_Set := Set;
		begin
			if Set (Gremlin) = 0 or else
				Monster_Side /= Gremlin
			then
				Process_Doctor (Set, Village_Side_Superman_Count);
			end if;
			if Village_Side_Superman_Count > 0 then
				Set_2 (Hunter) := 1;
				Process_Doctor (Set_2, Village_Side_Superman_Count - 1);
			end if;
		end Process_Hunter;
		
		procedure Process_Astronomer (
			Set : in Role_Set;
			Village_Side_Superman_Count : in Village_Side_Superman_Count_Type)
		is
			Set_2 : Role_Set := Set;
		begin
			if Village_Side_Superman_Count = 0 or else
				Teaming not in Hidings
			then
				Process_Hunter (Set, Village_Side_Superman_Count);
			end if;
			if Village_Side_Superman_Count > 0 then
				Set_2 (Astronomer) := 1;
				Process_Hunter (Set_2, Village_Side_Superman_Count - 1);
			end if;
		end Process_Astronomer;
		
		procedure Process_Vampires_And_Servant (
			Set : in Role_Set;
			Village_Side_Superman_Count : in Village_Side_Superman_Count_Type;
			Vampire_Count : in Vampire_Count_Type;
			Servant_Count : in Servant_Count_Type)
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
			if Servant_Count > 0 then
				Set_2 (Servant) := Servant_Count;
			end if;
			Process_Astronomer (Set_2, Village_Side_Superman_Count);
		end Process_Vampires_And_Servant;
		
		procedure Process_Vampire_Side (
			Set : in Role_Set;
			Village_Side_Superman_Count : in Village_Side_Superman_Count_Type;
			Vampire_Count : in Vampire_Count_Type;
			Servant_Count : in Servant_Count_Type)
		is
			Vampire_Count_2 : Vampire_Count_Type := Vampire_Count;
			Servant_Count_2 : Servant_Count_Type := Servant_Count;
		begin
			if Monster_Side = Shuffling then
				Vampire_Count_2 := Vampire_Count_2 + Servant_Count_2;
				Servant_Count_2 := 0;
				Process_Vampires_And_Servant (
					Set,
					Village_Side_Superman_Count,
					Vampire_Count_2,
					Servant_Count_2);
				Vampire_Count_2 := Vampire_Count_2 - 1;
				Servant_Count_2 := 1;
				Process_Vampires_And_Servant (
					Set,
					Village_Side_Superman_Count,
					Vampire_Count_2,
					Servant_Count_2);
			else
				Process_Vampires_And_Servant (
					Set,
					Village_Side_Superman_Count,
					Vampire_Count,
					Servant_Count);
			end if;
		end Process_Vampire_Side;
		
		procedure Process_Gremlin (
			Set : in Role_Set;
			Village_Side_Superman_Count : in Village_Side_Superman_Count_Type;
			Vampire_Count : in Vampire_Count_Type;
			Servant_Count : in Servant_Count_Type;
			Gremlin_Count : in Gremlin_Count_Type)
		is
			Set_2 : Role_Set := Set;
		begin
			if Gremlin_Count > 0 then
				Set_2 (Gremlin) := Gremlin_Count;
			end if;
			Process_Vampire_Side (
				Set_2,
				Village_Side_Superman_Count,
				Vampire_Count,
				Servant_Count);
		end Process_Gremlin;
		
		Village_Side_Superman_Count : Village_Side_Superman_Count_Type;
		Vampire_Count : Vampire_Count_Type;
		Servant_Count : Servant_Count_Type := 0;
		Gremlin_Count : Gremlin_Count_Type := 0;
	begin
		if Execution in From_Seconds then
			People_Count_2 := People_Count_2 - 1;
		end if;
		case Teaming is
			when Low_Density =>
				if People_Count_2 >= 16 then
					Village_Side_Superman_Count := 6;
				elsif People_Count_2 >= 14 then
					Village_Side_Superman_Count := 5;
				elsif People_Count_2 >= 12 then
					Village_Side_Superman_Count := 4;
				elsif People_Count_2 >= 10 then
					Village_Side_Superman_Count := 3;
				elsif People_Count_2 >= 8 then
					Village_Side_Superman_Count := 2;
				else
					Village_Side_Superman_Count := 1;
				end if;
				if People_Count_2 >= 15 then
					Vampire_Count := 3;
					Servant_Count := 1;
				elsif People_Count_2 >= 11 then
					Vampire_Count := 2;
					Servant_Count := 1;
				elsif People_Count_2 >= 7 then
					Vampire_Count := 2;
				else
					Vampire_Count := 1;
				end if;
				if People_Count_2 >= 16 then
					Gremlin_Count := 1;
				end if;
			when Shuffling_Headless =>
				if People_Count_2 >= 15 then
					Village_Side_Superman_Count := 6;
				elsif People_Count_2 >= 12 then
					Village_Side_Superman_Count := 5;
				elsif People_Count_2 >= 10 then
					Village_Side_Superman_Count := 4;
				elsif People_Count_2 >= 8 then
					Village_Side_Superman_Count := 3;
				elsif People_Count_2 = 7 then
					Village_Side_Superman_Count := 2;
				else
					Village_Side_Superman_Count := 1;
				end if;
				if People_Count_2 >= 14 then
					Vampire_Count := 3;
					Servant_Count := 1;
				elsif People_Count_2 >= 9 then
					Vampire_Count := 2;
					Servant_Count := 1;
				elsif People_Count_2 >= 7 then
					Vampire_Count := 2;
				else
					Vampire_Count := 1;
				end if;
				if People_Count_2 >= 16 then
					Gremlin_Count := 1;
				end if;
			when Shuffling_Euro =>
				if People_Count_2 >= 16 then
					Village_Side_Superman_Count := 4;
				elsif People_Count_2 >= 10 then
					Village_Side_Superman_Count := 3;
				elsif People_Count_2 >= 8 then
					Village_Side_Superman_Count := 2;
				else
					Village_Side_Superman_Count := 1;
				end if;
				if People_Count_2 >= 15 then
					Vampire_Count := 3;
					Servant_Count := 1;
				elsif People_Count_2 >= 9 then
					Vampire_Count := 2;
					Servant_Count := 1;
				elsif People_Count_2 >= 7 then
					Vampire_Count := 2;
				else
					Vampire_Count := 1;
				end if;
				if People_Count_2 >= 11 then
					Gremlin_Count := 1;
				end if;
			when Shuffling | Hiding =>
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
			when Shuffling_Gremlin | Hiding_Gremlin =>
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
				if People_Count_2 >= 11 then
					Gremlin_Count := 1;
				end if;
		end case;
		if Teaming in Hidings then
			Village_Side_Superman_Count := Village_Side_Superman_Count + 1;
		end if;
		declare
			Zero_Set : constant Role_Set := (others => 0);
		begin
			Process_Gremlin (Zero_Set,
				Village_Side_Superman_Count,
				Vampire_Count,
				Servant_Count,
				Gremlin_Count);
		end;
		return Result (1 .. Last);
	end Possibilities;
	
end Tabula.Vampires.Villages.Teaming;
