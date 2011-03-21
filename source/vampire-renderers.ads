-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Calendar;
with Ada.Streams;
with Web;
with Tabula.Villages.Lists;
with Vampire.Configurations;
with Vampire.Villages;
private with Web.Producers;
package Vampire.Renderers is
	
	type Configuration_Access is access constant Configurations.Template_Names_Type;
	
	type Renderer(Configuration : Configuration_Access) is tagged null record;
	
	-- Page Generating
	
	procedure Refresh_Page(
		Object : in Renderer; 
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		URI : in String);
	
private
	
	procedure Produce(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		File_Name : in String;
		Handler : not null access procedure(Output : not null access Ada.Streams.Root_Stream_Type'Class; Tag : in String; Contents : Web.Producers.Template));
	
	procedure Link_Style_Sheet(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class);
	
	procedure Handle_List(
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String;
		Template : in Web.Producers.Template;
		Object : in Renderer;
		Summaries : in Tabula.Villages.Lists.Summary_Maps.Map;
		Log_Limits : in Natural;
		User_Id, User_Password : in String);
	
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
		Village_Id : Tabula.Villages.Village_Id := Tabula.Villages.Invalid_Village_Id;
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
		State : Tabula.Villages.Village_State);
	
	function HTML_Version(Object : in Renderer) return Web.HTML_Version;
	
	function Name (Person : Vampire.Villages.Person_Type) return String;
	
	procedure Handle_Users(
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String;
		Template : in Web.Producers.Template;
		Object : in Renderer;
		Village_Id : in Tabula.Villages.Village_Id := Tabula.Villages.Invalid_Village_Id;
		User_Id : in String;
		User_Password : in String);
	
	procedure Handle_Villages(
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String;
		Template : in Web.Producers.Template;
		Object : in Renderer;
		Village_Id : in Tabula.Villages.Village_Id;
		Village : in Vampire.Villages.Village_Type;
		Day : in Natural;
		User_Id : in String;
		User_Password : in String);
	
	procedure Handle_Messages(
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String;
		Template : in Web.Producers.Template;
		Object : in Renderer;
		Village_Id : in Tabula.Villages.Village_Id;
		Village : in Vampire.Villages.Village_Type;
		Day : in Natural;
		Message : in Vampire.Villages.Message;
		Time : in Ada.Calendar.Time;
		User_Id : in String;
		User_Password : in String);
	
	procedure Write(Output : not null access Ada.Streams.Root_Stream_Type'Class; Item : in Character)
		renames Character'Write;
	procedure Write(Output : not null access Ada.Streams.Root_Stream_Type'Class; Item : in String)
		renames String'Write;
	
	function To_String(X : Integer) return String;
	
end Vampire.Renderers;
