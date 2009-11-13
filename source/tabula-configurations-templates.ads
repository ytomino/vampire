-- The Village of Vampire by YT, このソースコードはNYSLです
with Tabula.Renderers;
package Tabula.Configurations.Templates is
	
	Configuration : constant Tabula.Renderers.Configuration_Access := new Tabula.Renderers.Configuration'(
		Output_Directory => Base_Directory'Access,
		Image_Directory => Image_Directory'Access,
		Log_Directory => Villages_HTML_Directory'Access,
		Style_Sheet_File_Name => Style_Sheet_File_Name'Access,
		Background_Image_File_Name => Background_Image_File_Name'Access,
		Template_Index_File_Name => new String'("template-index.html"),
		Template_List_File_Name => new String'("template-list.html"),
		Template_Users_File_Name => new String'("template-users.html"),
		Template_Register_File_Name => new String'("template-register.html"),
		Template_User_File_Name => new String'("template-user.html"),
		Template_Village_File_Name => new String'("template-village.html"),
		Template_Preview_File_Name => new String'("template-preview.html"),
		Template_Target_File_Name => new String'("template-target.html"),
		Template_Message_File_Name => new String'("template-message.html"),
		Template_Error_File_Name => new String'("template-error.html"));
	
	Simple_Configuration : constant Tabula.Renderers.Configuration_Access := new Tabula.Renderers.Configuration'(
		Output_Directory => Villages_HTML_Directory'Access,
		Image_Directory => Image_Directory'Access,
		Log_Directory => Villages_HTML_Directory'Access,
		Style_Sheet_File_Name => Style_Sheet_File_Name'Access,
		Background_Image_File_Name => Background_Image_File_Name'Access,
		Template_Index_File_Name => new String'("template-index-simple.html"),
		Template_List_File_Name => new String'("template-list-simple.html"),
		Template_Users_File_Name => new String'("template-users-simple.html"),
		Template_Register_File_Name => new String'("template-register-simple.html"),
		Template_User_File_Name => new String'("template-user-simple.html"),
		Template_Village_File_Name => new String'("template-village-simple.html"),
		Template_Preview_File_Name => new String'("template-preview-simple.html"),
		Template_Target_File_Name => new String'("template-target-simple.html"),
		Template_Message_File_Name => new String'("template-message-simple.html"),
		Template_Error_File_Name => new String'("template-error-simple.html"));
	
end Tabula.Configurations.Templates;
