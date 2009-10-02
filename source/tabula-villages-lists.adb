-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Strings.Unbounded;
with Tabula.String_Lists;
with Tabula.Villages.Lists;
use type Ada.Strings.Unbounded.Unbounded_String;
use Tabula.String_Lists;
use Tabula.Villages.Lists.Village_Lists;
package body Tabula.Villages.Lists is
	
	function "<" (L, R : Village_List_Item) return Boolean is
	begin
		return L.Id < R.Id;
	end "<";
	
	function Joined(User_Id : String; List : Tabula.Villages.Lists.Village_Lists.Vector; Long_Only : Boolean) return Boolean is
	begin
		for I in List.First_Index .. List.Last_Index loop
			declare
				V : Tabula.Villages.Lists.Village_List_Item renames List.Constant_Reference(I).Element.all;
			begin
				if not Long_Only or else V.Day_Duration >= 24 * 60 * 60.0 then
					if V.State <= Villages.Opened then
						declare
							J : String_Lists.Cursor := V.People.First;
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
		end loop;
		return False;
	end Joined;
	
	function Created(User_Id : String; List : Tabula.Villages.Lists.Village_Lists.Vector;
		Excluding : Tabula.Villages.Lists.Village_Id) return Boolean is
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
