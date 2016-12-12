-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Calendar;
package Tabula.Debug is
	
	procedure Hook (
		Name : not null Static_String_Access;
		Time : in Ada.Calendar.Time);
	
end Tabula.Debug;
