-- The Village of Vampire by YT, このソースコードはNYSLです
package body Tabula.Casts is
	use type Ada.Strings.Unbounded.Unbounded_String;
	
	function Find (Container : Groups.Vector; Group : Integer) return Groups.Cursor is
	begin
		for I in Container.First_Index .. Container.Last_Index loop
			if Container.Constant_Reference (I).Group = Group then
				return I;
			end if;
		end loop;
		return Groups.No_Element;
	end Find;
	
	function Is_Empty (Item : Person) return Boolean is
	begin
		return Item.Name.Is_Null;
	end Is_Empty;
	
	function Is_Empty (Item : Work) return Boolean is
	begin
		return Item.Name.Is_Null;
	end Is_Empty;
	
	function Find (Works : Casts.Works.Vector; Name : String) return Casts.Works.Cursor is
	begin
		for I in Works.First_Index .. Works.Last_Index loop
			if Works.Constant_Reference (I).Name = Name then
				return I;
			end if;
		end loop;
		return Casts.Works.No_Element;
	end Find;
	
	procedure Exclude_Person (Cast : in out Casts.Cast_Collection; Name : in String; Group : in Integer) is
	begin
		for IP in Cast.People.First_Index .. Cast.People.Last_Index loop
			if Cast.People.Constant_Reference(IP).Name = Name or else Cast.People.Constant_Reference(IP).Group /= Group then
				Cast.People.Reference(IP).Name := Ada.Strings.Unbounded.Null_Unbounded_String;
			end if;
		end loop;
	end Exclude_Person;
	
	procedure Exclude_Work (Cast : in out Casts.Cast_Collection; Name : in String) is
	begin
		for IW in Cast.Works.First_Index .. Cast.Works.Last_Index loop
			if Cast.Works.Constant_Reference(IW).Name = Name then
				Cast.Works.Reference(IW).Name := Ada.Strings.Unbounded.Null_Unbounded_String;
				exit;
			end if;
		end loop;
	end Exclude_Work;
	
end Tabula.Casts;
