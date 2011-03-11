-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Formatting;
package body Tabula.Calendar is
	
	function Image_00 is new Ada.Formatting.Integer_Image (
		Natural,
		Form => Ada.Formatting.Simple,
		Minus_Sign => Ada.Formatting.None,
		Zero_Sign => Ada.Formatting.None,
		Plus_Sign => Ada.Formatting.None,
		Width => 2,
		Padding => '0');
	
	function Image (Time_Zone : Ada.Calendar.Time_Zones.Time_Offset) return String is
		A : Natural := abs Integer (Time_Zone);
		H : Natural := A / 60;
		M : Natural := A rem 60;
	begin
		return Result : String (1 .. 9) do
			Result (1 .. 3) := "GMT";
			if Time_Zone < 0 then
				Result (4) := '-';
			else
				Result (4) := '+';
			end if;
			Result (5 .. 6) := Image_00 (H);
			Result (7) := ':';
			Result (8 .. 9) := Image_00 (M);
		end return;
	end Image;
	
end Tabula.Calendar;
