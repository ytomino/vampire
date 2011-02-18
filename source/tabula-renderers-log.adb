-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Tabula.Configurations.Templates;
with Tabula.Renderers.Village_Page;
with Tabula.String_Lists;
with Tabula.Vampires.Villages.Load;
package body Tabula.Renderers.Log is
	
	function "+" (S : Ada.Strings.Unbounded.Unbounded_String) return String renames Ada.Strings.Unbounded.To_String;
	
	function Load_Info (Id : in Villages.Village_Id) return Villages.Lists.Village_List_Item is
		Village : Vampires.Villages.Village_Type;
		Id_List : String_Lists.List;
	begin
		Vampires.Villages.Load (Id, Village, Info_Only => True);
		for I in Village.People.First_Index .. Village.People.Last_Index loop
			String_Lists.Append (Id_List, +Village.People.Constant_Reference(I).Element.Id);
		end loop;
		return (
			Id => Id, 
			Name => Village.Name, 
			By => Village.By,
			Day_Duration => Village.Day_Duration,
			People => Id_List, 
			Today => Village.Today,
			State => Village.State);
	end Load_Info;
	
	procedure Create_Log (Id : Villages.Village_Id) is
		Village : aliased Vampires.Villages.Village_Type;
	begin
		Vampires.Villages.Load (Id, Village, Info_Only => False);
		for Day in 0 .. Village.Today loop
			declare
				Renderer : Log.Renderer(Configurations.Templates.Configuration);
				Log_File_Name : String renames Ada.Directories.Compose(Configurations.Villages_HTML_Directory,
					Id & "-" & To_String(Day) & ".html");
				Output : Ada.Streams.Stream_IO.File_Type;
			begin
				Ada.Streams.Stream_IO.Create(Output, Ada.Streams.Stream_IO.Out_File, Log_File_Name);
				Village_Page (
					Renderer,
					Ada.Streams.Stream_IO.Stream(Output),
					Id,
					Village'Access, 
					Day => Day, 
					User_Id => "",
					User_Password => "");
				Ada.Streams.Stream_IO.Close(Output);
			end;
		end loop;
	end Create_Log;
	
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
		Template : in Web.Producers.Template;
		User_Id : in String;
		User_Password : in String;
		Link_To_User_Page : Boolean) is
	begin
		null;
	end User_Panel;
	
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
		User_Page : Boolean := False) is
	begin
		if User_Page then
			raise Program_Error;
		elsif Village_Id = Villages.Invalid_Village_Id then
			Write(Output, """../""");
		else
			Write(Output, """./");
			Write(Output, Village_Id);
			Write(Output, "-");
			Write(Output, To_String(Integer'Max(0, Day)));
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
