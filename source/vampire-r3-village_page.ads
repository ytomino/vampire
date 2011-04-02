-- The Village of Vampire by YT, このソースコードはNYSLです
with Vampire.Villages;
procedure Vampire.R3.Village_Page (
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Form : in Forms.Root_Form_Type'Class;
	Template : in Web.Producers.Template;
	Current_Directory : in String;
	HTML_Directory : in String;
	Image_Directory : in String;
	Style_Sheet : in String;
	Background : in String;
	Relative_Role_Images : in Villages.Role_Images;
	Cast_File_Name : in String;
	Log : in Boolean;
	Village_Id : in Tabula.Villages.Village_Id;
	Village : in Villages.Village_Type;
	Day : in Natural;
	Showing_Range : in Tabula.Villages.Message_Range_Type := (
		First => Tabula.Villages.Message_Index'First,
		Last => Tabula.Villages.Message_Index'Last - 2);
	Editing : Villages.Message_Kind := Villages.Speech;
	Editing_Text : String := "";
	User_Id : in String;
	User_Password : in String);
