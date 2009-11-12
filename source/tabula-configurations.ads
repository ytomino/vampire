-- The Village of Vampire by YT, このソースコードはNYSLです
package Tabula.Configurations is
	pragma Preelaborate;
	
	Temporary_Directory : constant String := "temp";
	
	Lock_Name : aliased constant String := "temp/lock-vampire";
	Village_List_Cache_File_Name : constant String := "temp/cache-villages";
	Users_Log_File_Name : constant String := "temp/users-log";
	Short_Term_Village_Blocking_File_Name : constant String := "temp/disabled-short";
	
	Base_Directory : aliased constant String := "";
	
	Users_Directory : constant String := "users";
	Cast_File_Name : constant String := "cast";
	Villages_Data_Directory : aliased constant String := "villages/data";
	Villages_HTML_Directory : aliased constant String := "villages";
	
	Image_Directory : aliased constant String := "image";
	Style_Sheet_File_Name : aliased constant String := "style.css";
	Background_Image_File_Name : aliased constant String := "background.png";
	
	List_HTML_File_Name : constant String := "villages/index.html";
	List_RSS_File_Name : constant String := "villages/wanted.rdf";
	
end Tabula.Configurations;
