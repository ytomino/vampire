-- The Village of Vampire by YT, このソースコードはNYSLです
package body Tabula.Villages is
	
	function Option_Changed (Object : not null access constant Village) return Boolean is
		Result : Boolean := False;
		procedure Process (Item : in Root_Option_Item'Class) is
		begin
			if Item.Changed then
				Result := True;
			end if;
		end Process;
	begin
		Iterate (Village'Class (Object.all)'Access, Process'Access);
		return Result;
	end Option_Changed;
	
end Tabula.Villages;
