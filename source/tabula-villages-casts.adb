-- The Village of Vampire by YT, このソースコードはNYSLです
package body Tabula.Villages.Casts is
	
	overriding procedure Finalize(Object : in out Cast_Type) is
	begin
		Free(Object.People);
		Free(Object.Works);
	end Finalize;
	
	procedure Exclude_Taken(Cast : in out Casts.Cast_Type; Village : Village_Type) is
		use type Ada.Strings.Unbounded.Unbounded_String;
		use type Ada.Containers.Count_Type;
	begin
		if Village.People /= null then
			for Position in Village.People'Range loop
				declare
					P : Villages.Person_Type renames Village.People(Position);
				begin
					-- remove all duplicated characters
					for IP in Cast.People'Range loop
						if Cast.People(IP).Name = P.Name or else Cast.People(IP).Group /= P.Group then
							Cast.People(IP) := Default_Person;
						end if;
					end loop;
					-- remove one duplicated work
					for IW in Cast.Works'Range loop
						if Cast.Works(IW).Name = P.Work then
							Cast.Works(IW) := Casts.Default_Work;
							exit;
						end if;
					end loop;
				end;
			end loop;
		end if;
	end Exclude_Taken;

end Tabula.Villages.Casts;
