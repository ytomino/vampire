-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Streams;
with Web;
with Tabula.Villages;
with Tabula.Villages.Lists;
with Tabula.Users;
private with Web.Producers;
package Tabula.Renderers is
	
	type File_Name is not null access constant String;
	
	type Configuration is record
		Output_Directory : File_Name;
		Image_Directory : File_Name;
		Log_Directory : File_Name;
		Style_Sheet_File_Name : File_Name;
		Background_Image_File_Name : File_Name;
		Template_Index_File_Name : not null access constant String;
		Template_List_File_Name : not null access constant String;
		Template_Register_File_Name : not null access constant String;
		Template_User_File_Name : not null access constant String;
		Template_Village_File_Name : not null access constant String;
		Template_Preview_File_Name : not null access constant String;
		Template_Target_File_Name : not null access constant String;
		Template_Message_File_Name : not null access constant String;
		Template_Error_File_Name : not null access constant String;
	end record;
	
	type Configuration_Access is access constant Configuration;
	
	Speeches_By_Page : constant := 12;
	
	type Renderer(Configuration : Configuration_Access) is tagged null record;
	
	-- Info
	
	function Get_Village_Id(
		Object : Renderer; 
		Query_Strings : Web.Query_Strings) return Villages.Lists.Village_Id;
	
	procedure Get_Day(
		Object : in Renderer; 
		Village : in Villages.Village_Type; 
		Query_Strings : in Web.Query_Strings; 
		Day : out Natural);
	
	procedure Get_Range(
		Object : in Renderer; 
		Village : in Villages.Village_Type; 
		Day : in Natural;
		Query_Strings : in Web.Query_Strings; 
		First, Last : out Integer);
	
	function Get_User_Id(
		Object : Renderer; 
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie) return String;
	
	function Get_User_Password(
		Object : Renderer; 
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie) return String;
	
	procedure Set_User(
		Object : in Renderer; 
		Cookie : in out Web.Cookie;
		New_User_Id: in String;
		New_User_Password : in String);
	
	function Get_Text(
		Object : Renderer; 
		Inputs : Web.Query_Strings) return String;
	
	function Is_User_Page(
		Object : Renderer; 
		Query_Strings : Web.Query_Strings;
		Cookie : Web.Cookie) return Boolean;
	
	-- Page Generating
	
	procedure Refresh_Page(
		Object : in Renderer; 
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		URI : in String);
	
	procedure Index_Page(
		Object : in Renderer; 
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Village_List : in Villages.Lists.Village_Lists.Vector; 
		Muramura : Natural;
		User_Id: in String;
		User_Password : in String);
	
	procedure List_Page(
		Object : in Renderer; 
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Village_List : in Villages.Lists.Village_Lists.Vector);
	
	procedure Register_Page(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Village_Id : in Villages.Lists.Village_Id := Villages.Lists.Invalid_Village_Id;
		New_User_Id : String;
		New_User_Password : String);
	
	procedure User_Page(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Village_List : Villages.Lists.Village_Lists.Vector;
		User_Id : in String;
		User_Password : in String;
		User_Info : in Users.User_Info);
	
	procedure Village_Page(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Village_Id : Villages.Lists.Village_Id; 
		Village : Villages.Village_Type; 
		Day : Natural;
		First, Last : Integer := -1;
		User_Id : String;
		User_Password : String);
	
	procedure Preview_Page(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Village_Id : Villages.Lists.Village_Id;
		Village : Villages.Village_Type; 
		Message : Villages.Message;
		User_Id : String;
		User_Password : String);
	
	-- 医者と探偵の確認用ページ
	procedure Target_Page(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Village_Id : Villages.Lists.Village_Id;
		Village : Villages.Village_Type;
		Player : Natural;
		Target : Natural;
		User_Id : String;
		User_Password : String);
	
private
	
	procedure Produce(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		File_Name : in String;
		Handler : not null access procedure(Output : not null access Ada.Streams.Root_Stream_Type'Class; Tag : in String; Contents : Web.Producers.Template));
	
	procedure Link_Style_Sheet(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class);
	
	procedure User_Panel(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Template : in Web.Producers.Template;
		User_Id : in String;
		User_Password : in String;
		Link_To_User_Page : Boolean);
	
	procedure Link(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Village_Id : Villages.Lists.Village_Id := Villages.Lists.Invalid_Village_Id;
		Day : Integer := -1;
		First : Integer := -1;
		Last : Integer := -1;
		Latest : Integer := -1;
		Log : Boolean := False;
		User_Id : in String;
		User_Password : in String;
		User_Page: Boolean := False);
	
	procedure Link_Image(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		File_Name : in String);
	
	procedure Day_Name(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Day : Natural;
		Today : Natural;
		State : Villages.Village_State);
	
	function HTML_Version(Object : in Renderer) return Web.HTML_Version;
	
	procedure Write(Output : not null access Ada.Streams.Root_Stream_Type'Class; Item : in Character)
		renames Character'Write;
	procedure Write(Output : not null access Ada.Streams.Root_Stream_Type'Class; Item : in String)
		renames String'Write;
	
	function To_String(X : Integer) return String;
	
end Tabula.Renderers;
