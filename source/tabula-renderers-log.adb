-- The Village of Vampire by YT, このソースコードはNYSLです
with Ase.Editing;
package body Tabula.Renderers.Log is

	overriding procedure Link_Style_Sheet(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class) is
	begin
		Write(Output, """../");
		Write(Output, Object.Configuration.Style_Sheet_File_Name.all);
		Write(Output, '"');
	end Link_Style_Sheet;
	
	overriding procedure User_Panel(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Template : in Ase.Web.Producers.Template;
		User_Id : in String;
		User_Password : in String;
		Link_To_User_Page : Boolean) is
	begin
		null;
	end User_Panel;
	
	overriding procedure Link(
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
		User_Page : Boolean := False) is
	begin
		if User_Page then
			raise Program_Error;
		elsif Village_Id = Villages.Lists.Invalid_Village_Id then
			Write(Output, """../""");
		else
			Write(Output, """./");
			Write(Output, Village_Id);
			Write(Output, "-");
			Write(Output, Ase.Editing.Image(Item => Integer'Max(0, Day)));
			Write(Output, ".html""");
		end if;
	end Link;
	
	overriding procedure Link_Image(
		Object : in Renderer;
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		File_Name : in String) is
	begin
		Write(Output, """../");
		Write(Output, Object.Configuration.Image_Directory.all);
		Write(Output, "/");
		Write(Output, File_Name);
		Write(Output, '"');
	end Link_Image;

end Tabula.Renderers.Log;
