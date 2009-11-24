-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Containers;
procedure Tabula.Vampires.Villages.Shuffle(
	People : in out Villages.People.Vector;
	Victim : access Villages.Person_Role;
	Teaming : in Teaming_Mode;
	Monster_Side : in Monster_Side_Mode;
	Appearance : in Role_Appearances;
	Generator : not null access Ada.Numerics.MT19937.Generator)
is
	use type Ada.Containers.Count_Type;
	use type Casts.Sex_Kind;
	
	subtype Village_Side_Count_Type is Natural range 1 .. 6;
	subtype Vampire_Count_Type is Natural range 1 .. 3;
	
	People_Count : constant Ada.Containers.Count_Type := People.Length;
	subtype People_Index is Integer range People.First_Index .. People.Last_Index;
	package People_Random is new Ada.Numerics.MT19937.Discrete_Random(People_Index);
	
	type Role_Set is array (Person_Role) of Boolean;
	pragma Pack(Role_Set);
	
	type Role_Counts is array (Person_Role) of Natural;
	
	function Ready_Role_Set return Role_Counts is
		Result : Role_Counts := (others => 0);
		Role_Count : Natural := 0;
		
		procedure Push(Role : Person_Role);
		pragma Inline_Always(Push);
		procedure Push(Role : Person_Role) is
		begin
			Result(Role) := Result(Role) + 1;
			Role_Count := Role_Count + 1;
		end Push;
		
		subtype I60 is Integer range 1 .. 60;
		package R60 is new Ada.Numerics.MT19937.Discrete_Random(I60);
		
		function Random_Matrix_Role return Matrix_Role is
			package RR is new Ada.Numerics.MT19937.Discrete_Random(Matrix_Role);
		begin
			loop 
				declare
					Role : constant Matrix_Role := RR.Random(Generator);
				begin
					if Result(Role) = 0 then
						return Role;
					end if;
				end;
			end loop;
		end Random_Matrix_Role;
		
		function Random_Night_Role return Night_Role is
			package RR is new Ada.Numerics.MT19937.Discrete_Random(Night_Role);
			Role : Night_Role := RR.Random(Generator);
		begin
			pragma Assert(Result(Role) = 0); -- 先立って1回だけ呼ばれる
			if Role = Hunter and then Result(Unfortunate_Inhabitant) > 0 then
				Role := RR.Random(Generator);
			end if;
			return Role;
		end Random_Night_Role;
		
		function Random_Daytime_Role return Daytime_Role is
			package RR is new Ada.Numerics.MT19937.Discrete_Random(Daytime_Role);
		begin
			loop
				declare
					Role : constant Daytime_Role := RR.Random(Generator);
				begin
					if Result(Role) = 0 then
						return Role;
					end if;
				end;
			end loop;
		end Random_Daytime_Role;
		
		Village_Side_Count : Village_Side_Count_Type;
		Vampire_Count : Vampire_Count_Type;
		Servant_Existing : Natural range 0 .. 1 := 0;
		Gremlin_Existing : Boolean := False;
	begin
		case Teaming is
			when Low_Density =>
				if People_Count >= 16 then
					Village_Side_Count := 6;
				elsif People_Count >= 14 then
					Village_Side_Count := 5;
				elsif People_Count >= 12 then
					Village_Side_Count := 4;
				elsif People_Count >= 10 then
					Village_Side_Count := 3;
				elsif People_Count >= 8 then
					Village_Side_Count := 2;
				else
					Village_Side_Count := 1;
				end if;
				if People_Count >= 15 then
					Vampire_Count := 3;
					Servant_Existing := 1;
				elsif People_Count >= 11 then
					Vampire_Count := 2;
					Servant_Existing := 1;
				else
					Vampire_Count := 2;
				end if;
				if People_Count >= 16 then
					Gremlin_Existing := True;
				end if;
			when Shuffling_Headless =>
				if People_Count >= 15 then
					Village_Side_Count := 6;
				elsif People_Count >= 12 then
					Village_Side_Count := 5;
				elsif People_Count >= 10 then
					Village_Side_Count := 4;
				elsif People_Count >= 8 then
					Village_Side_Count := 3;
				else
					Village_Side_Count := 2;
				end if;
				if People_Count >= 14 then
					Vampire_Count := 3;
					Servant_Existing := 1;
				elsif People_Count >= 9 then
					Vampire_Count := 2;
					Servant_Existing := 1;
				else
					Vampire_Count := 2;
				end if;
				if People_Count >= 16 then
					Gremlin_Existing := True;
				end if;
			when Shuffling_Euro =>
				if People_Count >= 16 then
					Village_Side_Count := 4;
				elsif People_Count >= 10 then
					Village_Side_Count := 3;
				elsif People_Count >= 8 then
					Village_Side_Count := 2;
				else
					Village_Side_Count := 1;
				end if;
				if People_Count >= 15 then
					Vampire_Count := 3;
					Servant_Existing := 1;
				elsif People_Count >= 9 then
					Vampire_Count := 2;
					Servant_Existing := 1;
				else
					Vampire_Count := 2;
				end if;
				if People_Count >= 11 then
					Gremlin_Existing := True;
				end if;
			when Shuffling | Hiding =>
				if People_Count >= 15 then
					Village_Side_Count := 5;
				elsif People_Count >= 13 then
					Village_Side_Count := 4;
				elsif People_Count >= 10 then
					Village_Side_Count := 3;
				elsif People_Count >= 8 then
					Village_Side_Count := 2;
				else
					Village_Side_Count := 1;
				end if;
				if People_Count >= 14 then
					Vampire_Count := 3;
					Servant_Existing := 1;
				elsif People_Count >= 13 then
					Vampire_Count := 3;
				elsif People_Count >= 9 then
					Vampire_Count := 2;
					Servant_Existing := 1;
				elsif People_Count >= 8 then
					Vampire_Count := 2;
				else
					Vampire_Count := 1;
					Servant_Existing := 1;
				end if;
				if People_Count >= 16 then
					Gremlin_Existing := True;
				end if;
			when Shuffling_Gremlin | Hiding_Gremlin =>
				if People_Count >= 15 then
					Village_Side_Count := 5;
				elsif People_Count >= 13 then
					Village_Side_Count := 4;
				elsif People_Count >= 10 then
					Village_Side_Count := 3;
				elsif People_Count >= 8 then
					Village_Side_Count := 2;
				else
					Village_Side_Count := 1;
				end if;
				if People_Count >= 14 then
					Vampire_Count := 3;
					Servant_Existing := 1;
				elsif People_Count >= 13 then
					Vampire_Count := 3;
				elsif People_Count >= 9 then
					Vampire_Count := 2;
					Servant_Existing := 1;
				elsif People_Count >= 8 then
					Vampire_Count := 2;
				else
					Vampire_Count := 1;
					Servant_Existing := 1;
				end if;
				if People_Count >= 11 then
					Gremlin_Existing := True;
				end if;
		end case;
		if Teaming in Hidings then
			Village_Side_Count := Village_Side_Count + 1;
		end if;
		-- Monster Side(Pre)
		if Monster_Side = Shuffling then
			if Servant_Existing = 0 then
				Vampire_Count := Vampire_Count - 1;
				Servant_Existing := 1;
			end if;
			declare
				Border : Integer;
			begin
				case Vampire_Count is
					when 3 => Border := -1;
					when 2 => Border := 20; -- QJ or QS
					when 1 => Border := 30; -- Q or S
				end case;
				if R60.Random(Generator) <= Border then
					Vampire_Count := Vampire_Count + 1;
					Servant_Existing := 0;
				end if;
			end;
		end if;
		if Monster_Side = Gremlin and then (Servant_Existing /= 0) and then not Gremlin_Existing then
			Servant_Existing := 0;
			Gremlin_Existing := True;
		end if;
		-- Village Side
		case Village_Side_Count is
			when 6 =>
				if Male_And_Female (People) then
					if (R60.Random(Generator) >= 30 and then Appearance(Unfortunate_Inhabitant) /= Force)
						or else Appearance(Unfortunate_Inhabitant) = None
					then
						Push(Sweetheart_M);
						Push(Sweetheart_F);
					else
						Push(Lover);
						Push(Unfortunate_Inhabitant);
					end if;
				elsif Appearance(Unfortunate_Inhabitant) /= None then
					Push(Unfortunate_Inhabitant);
				end if;
				Push(Detective);
				Push(Astronomer);
				Push(Doctor);
				Push(Hunter);
			when 5 =>
				if Male_And_Female (People) then
					if R60.Random(Generator) <= 42 then -- 5/7
						if Vampire_Count = 3 and then R60.Random(Generator) <= 40 then -- 4/6
							Push(Sweetheart_M);
							Push(Sweetheart_F);
						else
							if Vampire_Count = 3 or else R60.Random(Generator) <= 50 then -- "50(5/6)"は出現率調整
								Push(Lover);
							end if;
						end if;
					end if;
				end if;
				if Appearance(Unfortunate_Inhabitant) /= None 
					and then (Role_Count = 0
					or else Vampire_Count <= 2
					or else R60.Random(Generator) <= 42 * 5 / 6 -- "5/6"は出現率調整
					or else Appearance(Unfortunate_Inhabitant) = Force)
				then
					Push(Unfortunate_Inhabitant);
				end if;
				if Gremlin_Existing then
					Push(Hunter);
					Push(Astronomer);
				elsif Victim /= null or else Teaming in Hidings then
					Push(Astronomer);
				else
					Push(Random_Night_Role);
				end if;
				while Role_Count < 5 loop
					Push(Random_Matrix_Role);
				end loop;
			when 4 =>
				if Male_And_Female (People) then
					if R60.Random(Generator) <= 34 then -- 4/7
						if Vampire_Count = 3 and then R60.Random(Generator) <= 30 then -- 3/6
							Push(Sweetheart_M);
							Push(Sweetheart_F);
						else
							if Vampire_Count = 3 or else R60.Random(Generator) <= 40 then -- "40(4/6)"は出現率調整
								Push(Lover);
							end if;
						end if;
					end if;
				end if;
				if Appearance(Unfortunate_Inhabitant) /= None
					and then ((Vampire_Count <= 2 and then Role_Count = 0)
					or else R60.Random(Generator) <= 34 * 4 / 6 -- "4/6"は出現率調整
					or else Appearance(Unfortunate_Inhabitant) = Force)
				then
					Push(Unfortunate_Inhabitant);
				end if;
				if Gremlin_Existing then
					Push(Hunter);
					Push(Astronomer);
				elsif Victim /= null or else Teaming in Hidings then
					Push(Astronomer);
				else
					Push(Random_Night_Role);
				end if;
				while Role_Count < 4 loop
					Push(Random_Matrix_Role);
				end loop;
			when 3 => 
				if Male_And_Female (People) then
					if R60.Random(Generator) <= 30 * 3 / 6 then -- 3/6, "3/6"は出現率調整
						Push(Lover);
					end if;
				end if;
				if Appearance(Unfortunate_Inhabitant) /= None 
					and then not Gremlin_Existing and then (
					R60.Random(Generator) <= 30 * 3 / 6 -- "3/6"は出現率調整
					or else Appearance(Unfortunate_Inhabitant) = Force)
				then
					Push(Unfortunate_Inhabitant);
				end if;
				if Gremlin_Existing then
					Push(Hunter);
					Push(Astronomer);
				elsif Victim /= null or else Teaming in Hidings then
					Push(Astronomer);
				else
					Push(Random_Night_Role);
				end if;
				while Role_Count < 3 loop
					if Vampire_Count + Servant_Existing >= 3 then
						Push(Random_Matrix_Role);
					else
						Push(Random_Daytime_Role); -- 使徒が出るまでは天猟は同時に出ない
					end if;
				end loop;
			when 2 =>
				if Gremlin_Existing then
					Push(Hunter);
				elsif Appearance(Unfortunate_Inhabitant) /= None
					and then (Vampire_Count >= 2 and then (
					R60.Random(Generator) <= 24 / 2 -- (2/5) / 2
					or else Appearance(Unfortunate_Inhabitant) = Force))
				then
					Push(Unfortunate_Inhabitant);
				end if;
				Push(Astronomer);
				if Role_Count < 2 then
					if Vampire_Count + Servant_Existing >= 3 then
						Push(Random_Matrix_Role);
					else
						Push(Random_Daytime_Role); -- 使徒が出るまでは天猟は同時に出ない
					end if;
				end if;
			when 1 =>
				Push(Astronomer);
		end case;
		-- 内訳不明では
		if Teaming in Hidings then
			-- 片想いの出現率を大幅に下げる
			if Result(Lover) > 0 
				and then Result(Unfortunate_Inhabitant) = 0
				and then R60.Random(Generator) <= 50
				and then Appearance(Unfortunate_Inhabitant) /= None
			then
				Result(Lover) := Result(Lover) - 1;
				Result(Unfortunate_Inhabitant) := Result(Unfortunate_Inhabitant) + 1;
			end if;
			-- 猟師もちょっとだけ下げる
			if Result(Hunter) > 0 
				and then Result(Unfortunate_Inhabitant) = 0
				and then R60.Random(Generator) <= 10
				and then Appearance(Unfortunate_Inhabitant) /= None
			then
				Result(Hunter) := Result(Hunter) - 1;
				Result(Unfortunate_Inhabitant) := Result(Unfortunate_Inhabitant) + 1;
			end if;
		end if;
		-- Lover-Loved
		if Result(Lover) > 0 then
			Push(Loved_Inhabitant);
		end if;
		-- Monster-Side(Push)
		Push(Vampire_K);
		if Vampire_Count >= 2 then
			Push(Vampire_Q);
			if Vampire_Count = 3 then
				Push(Vampire_J);
			end if;
		end if;
		if Servant_Existing = 1 then
			Push(Servant);
		end if;
		if Gremlin_Existing then
			Push(Gremlin);
		end if;
		pragma Assert(Result(Inhabitant) = 0);
		return Result;
	end Ready_Role_Set;
	
	function Request_To_Role_Set(Request : Requested_Role) return Role_Set is
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
	
	Roles : constant Role_Counts := Ready_Role_Set;
	Request : constant Request_Matrix := Get_Request_Matrix;
	type Assignment is array(People_Index) of Person_Role;
	
	function Random_Assignment return Assignment is
		Result : Assignment := (others => Inhabitant);
	begin
		for I in Person_Role loop
			for J in 1 .. Roles(I) loop
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
	
	function Evaluate(Candidacy : Assignment) return Integer is
		Bad : constant Integer := -1;
		Result : Integer := 0;
		Lover_Sex : Casts.Person_Sex := Casts.Male;
		Loved_Sex : Casts.Person_Sex := Casts.Female;
	begin
		for I in People_Index loop
			declare
				S : constant Casts.Person_Sex := People.Constant_Reference(I).Element.Sex;
			begin
				if (S = Casts.Male and Candidacy(I) = Sweetheart_F) or else
					(S = Casts.Female and Candidacy(I) = Sweetheart_M)
				then
					return Bad;
				elsif Candidacy(I) = Lover then
					Lover_Sex := S;
				elsif Candidacy(I) = Loved_Inhabitant then
					Loved_Sex := S;
				end if;
			end;
			if Request(I)(Candidacy(I)) then
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
				when Vampire_Role | Gremlin | Sweetheart_M | Sweetheart_F =>
					Victim.all := Inhabitant;
				when others =>
					Victim.all := Current.Assignment(Changing);
					Current.Assignment(Changing) := Inhabitant;
			end case;
		end;
	end if;
	-- 設定
	for I in People_Index loop
		People.Reference(I).Element.Role := Current.Assignment(I);
	end loop;
end Tabula.Vampires.Villages.Shuffle;
