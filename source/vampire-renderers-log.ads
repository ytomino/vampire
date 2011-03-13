-- The Village of Vampire by YT, このソースコードはNYSLです
with Tabula.Vampires.Villages;
package Tabula.Renderers.Log is
	
	type Renderer is new Renderers.Renderer with null record;
	
	function Summary (
		Village : Vampires.Villages.Village_Type)
		return Villages.Lists.Village_Summary;
	function Load_Summary (
		List : Villages.Lists.Villages_List;
		Id : Villages.Village_Id)
		return Villages.Lists.Village_Summary;
	procedure Create_Log (
		List : Villages.Lists.Villages_List;
		Id : in Villages.Village_Id);
	procedure Create_Index (
		Summaries : in Villages.Lists.Summary_Maps.Map;
		Update : in Boolean);
	
private
	
	overriding procedure Link_Style_Sheet(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class);
	
	overriding procedure User_Panel(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Template : in Web.Producers.Template;
		User_Id : in String;
		User_Password : in String;
		Link_To_User_Page : Boolean);
	
	overriding procedure Link(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Village_Id : Villages.Village_Id := Villages.Invalid_Village_Id;
		Day : Integer := -1;
		First : Integer := -1;
		Last : Integer := -1;
		Latest : Integer := -1;
		Log : Boolean := False;
		User_Id : in String;
		User_Password : in String;
		User_Page : Boolean := False);
	
	overriding procedure Link_Image(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		File_Name : in String);
	
end Tabula.Renderers.Log;
