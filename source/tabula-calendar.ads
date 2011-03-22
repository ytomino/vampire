-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Calendar.Time_Zones;
with Ada.Calendar.Formatting;
package Tabula.Calendar is
	use type Ada.Calendar.Time_Zones.Time_Offset;
	
	Null_Time : constant Ada.Calendar.Time;
	
	Time_Offset : constant Ada.Calendar.Time_Zones.Time_Offset := 9 * 60; -- GMT+9 日本
	
private
	
	Null_Time : constant Ada.Calendar.Time := Ada.Calendar.Formatting.Time_Of(
		Year => Ada.Calendar.Year_Number'First,
		Month => Ada.Calendar.Month_Number'First,
		Day => Ada.Calendar.Day_Number'First,
		Time_Zone => 0);
	
end Tabula.Calendar;
