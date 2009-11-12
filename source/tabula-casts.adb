-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Containers;
with Ada.Strings.Unbounded;
use type Ada.Containers.Count_Type;
use type Ada.Strings.Unbounded.Unbounded_String;
package body Tabula.Villages.Casts is
		
	procedure Exclude_Taken(Cast : in out Casts.Cast_Type; Village : Village_Type) is
	begin
		for Position in Village.People.First_Index .. Village.People.Last_Index loop
			declare
				P : Villages.Person_Type renames Village.People.Constant_Reference(Position).Element.all;
			begin
				-- remove all duplicated characters
				for IP in Cast.People.First_Index .. Cast.People.Last_Index loop
					if Cast.People.Constant_Reference(IP).Element.Name = P.Name or else Cast.People.Constant_Reference(IP).Element.Group /= P.Group then
						Cast.People.Reference(IP).Element.all := Default_Person;
					end if;
				end loop;
				-- remove one duplicated work
				for IW in Cast.Works.First_Index .. Cast.Works.Last_Index loop
					if Cast.Works.Constant_Reference(IW).Element.Name = P.Work then
						Cast.Works.Reference(IW).Element.all := Casts.Default_Work;
						exit;
					end if;
				end loop;
			end;
		end loop;
	end Exclude_Taken;

end Tabula.Villages.Casts;
