-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Calendar.Time_Zones;
private with Ada.Unchecked_Conversion;
package Tabula.Calendar is
	use type Ada.Calendar.Time_Zones.Time_Offset;
	
	Null_Time : constant Ada.Calendar.Time;
	
	Time_Offset : constant Ada.Calendar.Time_Zones.Time_Offset := 9 * 60; -- GMT+9 日本
	
private
	
	pragma Warnings(Off);
	function "+" is new Ada.Unchecked_Conversion(Long_Long_Integer, Ada.Calendar.Time);
	pragma Warnings(On);
	Null_Time : constant Ada.Calendar.Time := +Long_Long_Integer'(-7857734400000000000);

--	Null_Time : constant Ada.Calendar.Time := Ada.Calendar.Time_Of(
--		Year => Ada.Calendar.Year_Number'First, 
--		Month => Ada.Calendar.Month_Number'First,
--		Day => Ada.Calendar.Day_Number'First);
	
end Tabula.Calendar;
