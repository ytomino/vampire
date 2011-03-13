-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Web.RSS;
with Tabula.Configurations.Templates;
with Tabula.Renderers.List_Page;
with Tabula.Renderers.Village_Page;
with Tabula.Vampires.Villages.Load;
with Tabula.Vampires.Villages.Village_IO;
package body Tabula.Renderers.Log is
	use type Ada.Strings.Unbounded.Unbounded_String;
	use type Villages.Village_State;
	
	function "+" (S : Ada.Strings.Unbounded.Unbounded_String) return String renames Ada.Strings.Unbounded.To_String;
	
	function Summary (
		Village : Vampires.Villages.Village_Type)
		return Villages.Lists.Village_Summary
	is
		Id_List : Villages.Lists.User_Lists.List;
	begin
		for I in Village.People.First_Index .. Village.People.Last_Index loop
			Villages.Lists.User_Lists.Append (Id_List, +Village.People.Constant_Reference(I).Element.Id);
		end loop;
		return (
			Type_Code => +Vampires.Villages.Village_IO.YAML_Type,
			Name => Village.Name, 
			By => Village.By,
			Day_Duration => Village.Day_Duration,
			People => Id_List, 
			Today => Village.Today,
			State => Village.State);
	end Summary;
	
	function Load_Summary (
		List : Villages.Lists.Villages_List;
		Id : Villages.Village_Id)
		return Villages.Lists.Village_Summary
	is
		Village : Vampires.Villages.Village_Type;
	begin
		Vampires.Villages.Load (Villages.Lists.File_Name (List, Id), Village, Info_Only => True);
		return Summary (Village);
	end Load_Summary;
	
	procedure Create_Log (
		List : Villages.Lists.Villages_List;
		Id : in Villages.Village_Id)
	is
		Village : aliased Vampires.Villages.Village_Type;
	begin
		Vampires.Villages.Load (Villages.Lists.File_Name (List, Id), Village, Info_Only => False);
		for Day in 0 .. Village.Today loop
			declare
				Renderer : Log.Renderer(Configurations.Templates.Configuration);
				Output : Ada.Streams.Stream_IO.File_Type;
			begin
				Ada.Streams.Stream_IO.Create (
					Output,
					Ada.Streams.Stream_IO.Out_File,
					Villages.Lists.HTML_File_Name (List, Id, Day));
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
	
	procedure Create_Index (
		Summaries : in Villages.Lists.Summary_Maps.Map;
		Update : in Boolean)
	is
		procedure Make_Log_Index (Summaries : in Villages.Lists.Summary_Maps.Map) is
			Renderer : Renderers.Log.Renderer := Renderers.Log.Renderer'(Configuration => Configurations.Templates.Configuration);
			File: Ada.Streams.Stream_IO.File_Type :=
				Ada.Streams.Stream_IO.Create (Name => Configurations.Villages_Index_HTML_File_Name);
		begin
			Renderers.List_Page (Renderer, Ada.Streams.Stream_IO.Stream (File), Summaries);
			Ada.Streams.Stream_IO.Close (File);
		end Make_Log_Index;
		procedure Make_RSS (Summaries : in Villages.Lists.Summary_Maps.Map) is
			File: Ada.Streams.Stream_IO.File_Type :=
				Ada.Streams.Stream_IO.Create (
					Ada.Streams.Stream_IO.Out_File,
					Configurations.Villages_Index_RSS_File_Name);
			Stream : not null access Ada.Streams.Root_Stream_Type'Class :=
				Ada.Streams.Stream_IO.Stream (File);
		begin
			Web.RSS.RSS_Start (
				Stream,
				Title => "参加募集中の村 - The Village of Vampire",
				Description => "",
				Link => "../");
			declare
				I : Villages.Lists.Summary_Maps.Cursor := Summaries.Last;
			begin
				while Villages.Lists.Summary_Maps.Has_Element (I) loop
					declare
						Key : Villages.Village_Id
							renames Summaries.Constant_Reference (I).Key.all;
						Element : Villages.Lists.Village_Summary
							renames Summaries.Constant_Reference (I).Element.all;
					begin
						if Element.State = Villages.Prologue then
							Web.RSS.RSS_Item (
								Stream,
								Title => Element.Name.Constant_Reference.Element.all,
								Description => "",
								Link => "../?village=" & Key);
						end if;
					end;
					Villages.Lists.Summary_Maps.Previous (I);
				end loop;
			end;
			Web.RSS.RSS_End (Stream);
			Ada.Streams.Stream_IO.Close (File);
		end Make_RSS;
	begin
		if Update or else not Ada.Directories.Exists (Configurations.Villages_Index_HTML_File_Name) then
			Make_Log_Index (Summaries);
		end if;
		if Update or else not Ada.Directories.Exists (Configurations.Villages_Index_RSS_File_Name) then
			Make_RSS (Summaries);
		end if;
	end Create_Index;
	
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
