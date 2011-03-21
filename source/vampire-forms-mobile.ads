-- The Village of Vampire by YT, このソースコードはNYSLです
with iconv;
package Vampire.Forms.Mobile is
	
	Speeches_By_Page : constant := 12;
	
	type Form_Type is new Root_Form_Type with record
		Encoding : iconv.Encoding;
	end record;
	
	function Create return Form_Type;
	
	overriding function HTML_Version (Form : Form_Type) return Web.HTML_Version;
	
	overriding function Template_Set (Form : Form_Type) return Template_Set_Type;
	
	overriding procedure Write_In_HTML (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Form : in Form_Type;
		Item : in String;
		Pre : in Boolean := False);
	
	overriding procedure Write_In_Attribute (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Form : in Form_Type;
		Item : in String);
	
	overriding procedure Write_Link_To_Index_Page (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Form : in Form_Type;
		Current_Directory : in String;
		User_Id : in String;
		User_Password : in String);
	
	overriding procedure Write_Link_To_User_Page (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Form : in Form_Type;
		Current_Directory : in String;
		User_Id : in String;
		User_Password : in String);
	
	overriding procedure Write_Link_To_Village_Page (
		Stream : not null access Ada.Streams.Root_Stream_Type'Class;
		Form : in Form_Type;
		Current_Directory : in String;
		HTML_Directory : in String;
		Log : in Boolean;
		Village_Id : Tabula.Villages.Village_Id;
		Day : Integer := -1;
		First : Integer := -1;
		Last : Integer := -1;
		Latest : Integer := -1;
		User_Id : in String;
		User_Password : in String);
	
	overriding function Get_User_Id (
		Form : Form_Type;
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie)
		return String;
	
	overriding function Get_User_Password (
		Form : Form_Type;
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie)
		return String;
	
	overriding procedure Set_User (
		Form : in out Form_Type;
		Cookie : in out Web.Cookie;
		New_User_Id: in String;
		New_User_Password : in String);
	
	overriding function Is_User_Page (
		Form : Form_Type;
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie)
		return Boolean;
	
	overriding function Get_Village_Id (
		Form : Form_Type;
		Query_Strings : Web.Query_Strings)
		return Tabula.Villages.Village_Id;
	
	overriding function Get_Day (
		Form : Form_Type;
		Village : Villages.Village_Type; 
		Query_Strings : Web.Query_Strings)
		return Natural;
	
	overriding function Get_Range (
		Form : Form_Type;
		Village : Villages.Village_Type; 
		Day : Natural;
		Query_Strings : Web.Query_Strings)
		return Message_Range;
	
	overriding function Get_New_Village_Name (
		Form : Form_Type;
		Inputs : Web.Query_Strings)
		return String;
	
	overriding function Get_Text (
		Form : Form_Type;
		Inputs : Web.Query_Strings)
		return String;
	
end Vampire.Forms.Mobile;
