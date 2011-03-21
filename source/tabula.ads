-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Formatting;
package Tabula is
	pragma Pure;
	
	type Static_String_Access is access constant String;
	for Static_String_Access'Storage_Size use 0;
	
	-- string of Natural without spacing
	function Image is new Ada.Formatting.Integer_Image (
		Natural,
		Zero_Sign => Ada.Formatting.None,
		Plus_Sign => Ada.Formatting.None);
	
end Tabula;
