-- The Village of Vampire by YT, このソースコードはNYSLです
package body Tabula.Villages.Lists is

	use type Ada.Strings.Unbounded.Unbounded_String;

	function "<" (L, R : Village_List_Item) return Boolean is
	begin
		return L.Id < R.Id;
	end "<";

	function Joined(User_Id : String; List : Tabula.Villages.Lists.Village_Lists.Vector; Long_Only : Boolean) return Boolean is
		use Ase.Strings.Lists;
		use Tabula.Villages.Lists.Village_Lists;
		I : Tabula.Villages.Lists.Village_Lists.Cursor := List.First;
	begin
		while Has_Element(I) loop
			declare
				V : Tabula.Villages.Lists.Village_List_Item renames Element(I);
			begin
				if not Long_Only or else V.Day_Duration >= 24 * 60 * 60.0 then
					if V.State <= Villages.Opened then
						declare
							J : Ase.Strings.Lists.Cursor := V.People.First;
						begin
							while Has_Element(J) loop
								if Element(J) = User_Id then
									return True;
								end if;
								Next(J);
							end loop;
						end;
					end if;
				end if;
			end;
			Next(I);
		end loop;
		return False;
	end Joined;
	
	function Created(User_Id : String; List : Tabula.Villages.Lists.Village_Lists.Vector;
		Excluding : Tabula.Villages.Lists.Village_Id) return Boolean
	is
		use Ase.Strings.Lists;
		use Tabula.Villages.Lists.Village_Lists;
	begin
		for I in List.First_Index .. List.Last_Index loop
			declare
				V : Tabula.Villages.Lists.Village_List_Item renames Element(List, I);
			begin
				if V.State <= Villages.Opened and then V.By = User_Id
					and then V.Id /= Excluding
				then
					return True;
				end if;
			end;
		end loop;
		return False;
	end Created;

end Tabula.Villages.Lists;
