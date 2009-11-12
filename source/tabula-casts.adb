-- The Village of Vampire by YT, このソースコードはNYSLです
package body Tabula.Casts is
	use type Ada.Containers.Count_Type;
	use type Ada.Strings.Unbounded.Unbounded_String;
	
	procedure Exclude_Person (Cast : in out Casts.Cast_Collection; Name : String; Group : Integer) is
	begin
		for IP in Cast.People.First_Index .. Cast.People.Last_Index loop
			if Cast.People.Constant_Reference(IP).Element.Name = Name or else Cast.People.Constant_Reference(IP).Element.Group /= Group then
				Cast.People.Reference(IP).Element.all := Default_Person;
			end if;
		end loop;
	end Exclude_Person;
	
	procedure Exclude_Work (Cast : in out Casts.Cast_Collection; Name : String) is
	begin
		for IW in Cast.Works.First_Index .. Cast.Works.Last_Index loop
			if Cast.Works.Constant_Reference(IW).Element.Name = Name then
				Cast.Works.Reference(IW).Element.all := Casts.Default_Work;
				exit;
			end if;
		end loop;
	end Exclude_Work;
	
end Tabula.Casts;
