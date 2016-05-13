-- The Village of Vampire by YT, このソースコードはNYSLです
package body Tabula.Villages is
	use type Ada.Strings.Unbounded.Unbounded_String;
	use type Casts.Person_Sex;
	
	function Same_Id_And_Figure (Left, Right : Person_Type'Class) return Boolean is
	begin
		return Left.Id = Right.Id and then Left.Image = Right.Image;
	end Same_Id_And_Figure;
	
	function Option_Changed (Village : Village_Type) return Boolean is
		Result : Boolean := False;
		procedure Process (Item : in Root_Option_Item'Class) is
		begin
			if Item.Available and then Item.Changed then
				Result := True;
			end if;
		end Process;
	begin
		Iterate_Options (Village_Type'Class (Village), Process'Access); -- dyamic dispatch
		return Result;
	end Option_Changed;
	
	function Joined (Village : Village_Type; User_Id : String) return Person_Index'Base is
		Result : Person_Index'Base := No_Person;
		procedure Process (Index : in Person_Index; Item : in Person_Type'Class) is
		begin
			if Item.Id = User_Id then
				Result := Index;
			end if;
		end Process;
	begin
		Iterate_People (Village_Type'Class (Village), Process'Access);
		return Result;
	end Joined;
	
	function Already_Joined_As_Another_Sex (
		Village : Village_Type;
		User_Id : String;
		Sex : Casts.Person_Sex) return Boolean
	is
		Result : Boolean := False;
		procedure Process (Index : in Person_Index; Item : in Person_Type'Class) is
		begin
			if Item.Id = User_Id and then Item.Sex /= Sex then
				Result := True;
			end if;
		end Process;
	begin
		Iterate_Escaped_People (Village_Type'Class (Village), Process'Access);
		return Result;
	end Already_Joined_As_Another_Sex;
	
	function Male_And_Female (Village : Village_Type) return Boolean is
		Existing : array (Casts.Person_Sex) of Boolean := (False, False);
		procedure Process (Index : in Person_Index; Item : in Person_Type'Class) is
		begin
			Existing (Item.Sex) := True;
		end Process;
	begin
		Iterate_People (Village_Type'Class (Village), Process'Access);
		return Existing (Casts.Male) and then Existing (Casts.Female);
	end Male_And_Female;
	
	procedure Exclude_Taken(Cast : in out Casts.Cast_Collection; Village : in Village_Type) is
		procedure Process (Index : in Person_Index; Item : in Person_Type'Class) is
		begin
			-- remove all duplicated characters
			Casts.Exclude_Person (Cast, Item.Name.Constant_Reference, Item.Group);
			-- remove one duplicated work
			Casts.Exclude_Work (Cast, Item.Work.Constant_Reference);
		end Process;
	begin
		Iterate_People (Village_Type'Class (Village), Process'Access);
	end Exclude_Taken;
	
end Tabula.Villages;
