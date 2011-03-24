-- The Village of Vampire by YT, このソースコードはNYSLです
with Vampire.Forms;
with Vampire.R3;
with Vampire.Villages;
package Vampire.Configurations is
	
	Temporary_Directory : constant String := "temp";
	
	-- locking
	Lock_Name : aliased constant String := "temp/lock-vampire";
	
	-- for debug
	Debug_Log_File_Name : aliased constant String := "temp/debug-log.txt";
	
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
	Style_Sheet_File_Name : aliased constant String := "style.css";
	Image_Directory : aliased constant String := "image";
	
	Relative_Background_Image_File_Name : aliased constant String := "background.png";
	
	Relative_Role_Image_File_Names : aliased constant R3.Role_Images := (
		Villages.Gremlin =>
			new String'("gremlin.png"),
		Villages.Vampire_Role =>
			new String'("vampire.png"),
		Villages.Servant =>
			new String'("servant.png"),
		Villages.Inhabitant | Villages.Loved_Inhabitant | Villages.Unfortunate_Inhabitant =>
			new String'("inhabitant.png"),
		Villages.Detective =>
			new String'("detective.png"),
		Villages.Doctor =>
			new String'("doctor.png"),
		Villages.Astronomer =>
			new String'("astronomer.png"),
		Villages.Hunter =>
			new String'("hunter.png"),
		Villages.Lover | Villages.Sweetheart_M | Villages.Sweetheart_F =>
			new String'("sweetheart.png"));
	
	-- templates
	
	type Template_Names_Type is record
		Style_Sheet_File_Name : not null Static_String_Access;
		Image_Directory : not null Static_String_Access;
		Relative_Background_Image_File_Name : not null Static_String_Access;
		Relative_Role_Image_File_Names : not null access constant R3.Role_Images;
		Template_Index_File_Name : not null access constant String;
		Template_Register_File_Name : not null access constant String;
		Template_User_File_Name : not null access constant String;
		Template_User_List_File_Name : not null access constant String;
		Template_Village_File_Name : not null access constant String;
		Template_Preview_File_Name : not null access constant String;
		Template_Target_File_Name : not null access constant String;
		Template_Message_File_Name : not null access constant String;
		Template_Log_Index_File_Name : not null access constant String;
	end record;
	
	Template_Names : constant array (Forms.Template_Set_Type) of aliased Template_Names_Type := (
		Forms.For_Full => (
			Style_Sheet_File_Name => Style_Sheet_File_Name'Access,
			Image_Directory => Image_Directory'Access,
			Relative_Background_Image_File_Name => Relative_Background_Image_File_Name'Access,
			Relative_Role_Image_File_Names => Relative_Role_Image_File_Names'Access,
			Template_Index_File_Name => new String'("template-index.html"),
			Template_Register_File_Name => new String'("template-register.html"),
			Template_User_File_Name => new String'("template-user.html"),
			Template_User_List_File_Name => new String'("template-userlist.html"),
			Template_Village_File_Name => new String'("template-village.html"),
			Template_Preview_File_Name => new String'("template-preview.html"),
			Template_Target_File_Name => new String'("template-target.html"),
			Template_Message_File_Name => new String'("template-message.html"),
			Template_Log_Index_File_Name => new String'("template-logindex.html")),
		Forms.For_Mobile => (
			Style_Sheet_File_Name => Style_Sheet_File_Name'Access,
			Image_Directory => Image_Directory'Access,
			Relative_Background_Image_File_Name => Relative_Background_Image_File_Name'Access,
			Relative_Role_Image_File_Names => Relative_Role_Image_File_Names'Access,
			Template_Index_File_Name => new String'("template-index-simple.html"),
			Template_Register_File_Name => new String'("template-register-simple.html"),
			Template_User_File_Name => new String'("template-user-simple.html"),
			Template_User_List_File_Name => new String'("template-userlist-simple.html"),
			Template_Village_File_Name => new String'("template-village-simple.html"),
			Template_Preview_File_Name => new String'("template-preview-simple.html"),
			Template_Target_File_Name => new String'("template-target-simple.html"),
			Template_Message_File_Name => new String'("template-message-simple.html"),
			Template_Log_Index_File_Name => new String'("template-logindex-simple.html")));
	
	-- 携帯版で1ページに表示する発言数
	Speeches_Per_Page : constant := 12;
	
	-- cookie有効期限
	Cookie_Duration : constant Duration := 4 * 24 * 60 * 60 * 1.0;
	
	-- ムラムラスカウター(仮)
	Muramura_Duration : constant Duration := 24 * 60 * 60 * 1.0;
	
end Vampire.Configurations;
