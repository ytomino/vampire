-- The Village of Vampire by YT, このソースコードはNYSLです
package body Tabula.Casts is
	use type Ada.Containers.Count_Type;
	use type Ada.Strings.Unbounded.Unbounded_String;
	
	function Is_Empty (Item : Person) return Boolean is
	begin
		return Item.Name = Ada.Strings.Unbounded.Null_Unbounded_String;
	end Is_Empty;
	
	function Is_Empty (Item : Work) return Boolean is
	begin
		return Item.Name = Ada.Strings.Unbounded.Null_Unbounded_String;
	end Is_Empty;
	
	function Find (Works : Casts.Works.Vector; Name : String) return Casts.Works.Cursor is
	begin
		for I in Works.First_Index .. Works.Last_Index loop
			if Works.Constant_Reference (I).Element.Name = Name then
				return I;
			end if;
		end loop;
		return Casts.Works.No_Element;
	end Find;
	
	procedure Exclude_Person (Cast : in out Casts.Cast_Collection; Name : String; Group : Integer) is
	begin
		for IP in Cast.People.First_Index .. Cast.People.Last_Index loop
			if Cast.People.Constant_Reference(IP).Element.Name = Name or else Cast.People.Constant_Reference(IP).Element.Group /= Group then
				Cast.People.Reference(IP).Element.Name := Ada.Strings.Unbounded.Null_Unbounded_String;
			end if;
		end loop;
	end Exclude_Person;
	
	procedure Exclude_Work (Cast : in out Casts.Cast_Collection; Name : String) is
	begin
		for IW in Cast.Works.First_Index .. Cast.Works.Last_Index loop
			if Cast.Works.Constant_Reference(IW).Element.Name = Name then
				Cast.Works.Reference(IW).Element.Name := Ada.Strings.Unbounded.Null_Unbounded_String;
				exit;
			end if;
		end loop;
	end Exclude_Work;
	
end Tabula.Casts;
