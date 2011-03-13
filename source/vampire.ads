-- The Village of Vampire by YT, このソースコードはNYSLです
with Tabula;
pragma Unreferenced (Tabula);
package Vampire is
	pragma Pure;
	use Tabula;
	
	Default_Long_Day_Duration : constant Duration := 2 * 24 * 60 * 60 * 1.0;
	Default_Short_Day_Duration : constant Duration := 15 * 60.0;
	Default_Night_Duration  : constant Duration := 0.0;
	Epilogue_Min_Duration   : constant Duration := 60 * 60.0;
	Vote_Duration           : constant Duration := 5 * 60.0;
	Speech_Simultaneous     : constant Duration := 30.0;
	Cookie_Duration         : constant Duration := 4 * 24 * 60 * 60 * 1.0;
	Muramura_Duration       : constant Duration := 24 * 60 * 60 * 1.0;
	
	Minimum_Number_Of_Persons : constant :=  7;
	Maximum_Number_Of_Persons : constant := 16;
	
	Speech_Limit            : constant := 20;
	Encouraged_Speech_Limit : constant :=  2;
	Ghost_Limit             : constant := 10;
	Monologue_Limit         : constant := 10;
	
	Max_Length_Of_Message : constant := 1024;
	
end Vampire;
