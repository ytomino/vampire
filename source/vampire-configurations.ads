-- The Village of Vampire by YT, このソースコードはNYSLです
package Vampire.Configurations is
	
	Temporary_Directory : constant String := "temp";
	
	Lock_Name : aliased constant String := "temp/lock-vampire";
	
	-- for uses
	Users_Directory : aliased constant String := "users";
	
	-- temporary file for users
	Users_Log_File_Name : aliased constant String := "temp/users-log";
	
	-- for casts
	Cast_File_Name : constant String := "cast";
	
	-- for villages
	Villages_Data_Directory : aliased constant String := "villages/data";
	Villages_HTML_Directory : aliased constant String := "villages";
	Villages_Index_HTML_File_Name : aliased constant String := "villages/index.html";
	Villages_Index_RSS_File_Name : aliased constant String := "villages/wanted.rdf";
	
	-- temporary file for villages
	Villages_Cache_File_Name : aliased constant String := "temp/cache-villages";
	Villages_Blocking_Short_Term_File_Name : aliased constant String := "temp/disabled-short";
	
	-- for rendering
	Image_Directory : aliased constant String := "image";
	Style_Sheet_File_Name : aliased constant String := "style.css";
	Background_Image_File_Name : aliased constant String := "background.png";
	
	-- for debug
	Debug_Log_File_Name : aliased constant String := "temp/debug-log.txt";
	
end Vampire.Configurations;
