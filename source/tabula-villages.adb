-- The Village of Vampire by YT, このソースコードはNYSLです
package body Tabula.Villages is
	
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
	
end Tabula.Villages;
