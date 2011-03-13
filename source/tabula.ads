-- The Village of Vampire by YT, このソースコードはNYSLです
package Tabula is
	pragma Pure;
	
	type Static_String_Access is access constant String;
	for Static_String_Access'Storage_Size use 0;
	
end Tabula;
