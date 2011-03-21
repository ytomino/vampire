-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Web.RSS;
with Vampire.Configurations;
with Vampire.Forms;
with Vampire.Renderers.List_Page;
with Vampire.Renderers.Village_Page;
with Vampire.Villages.Load;
package body Vampire.Renderers.Log is
	use Tabula.Villages;
	use Tabula.Villages.Lists.Summary_Maps;
	use Tabula.Villages.Lists.User_Lists;
	use Villages;
	use type Ada.Strings.Unbounded.Unbounded_String;
	
	function Load_Summary (
		List : Tabula.Villages.Lists.Villages_List;
		Id : Tabula.Villages.Village_Id)
		return Tabula.Villages.Lists.Village_Summary
	is
		Village : Vampire.Villages.Village_Type;
	begin
		Vampire.Villages.Load (Lists.File_Name (List, Id), Village, Info_Only => True);
		return Tabula.Villages.Lists.Summary (Type_Code, Village);
	end Load_Summary;
	
	procedure Create_Log (
		List : Tabula.Villages.Lists.Villages_List;
		Id : in Tabula.Villages.Village_Id)
	is
		Village : aliased Vampire.Villages.Village_Type;
	begin
		Vampire.Villages.Load (Lists.File_Name (List, Id), Village, Info_Only => False);
		for Day in 0 .. Village.Today loop
			declare
				Renderer : Log.Renderer(Configurations.Template_Names (Forms.For_Full)'Access);
				Output : Ada.Streams.Stream_IO.File_Type;
			begin
				Ada.Streams.Stream_IO.Create (
					Output,
					Ada.Streams.Stream_IO.Out_File,
					Lists.HTML_File_Name (List, Id, Day));
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
		Summaries : in Tabula.Villages.Lists.Summary_Maps.Map;
		Update : in Boolean)
	is
		procedure Make_Log_Index (Summaries : in Lists.Summary_Maps.Map) is
			Renderer : Renderers.Log.Renderer := Renderers.Log.Renderer'(Configuration => Configurations.Template_Names (Forms.For_Full)'Access);
			File: Ada.Streams.Stream_IO.File_Type :=
				Ada.Streams.Stream_IO.Create (Name => Configurations.Villages_Index_HTML_File_Name);
		begin
			Renderers.List_Page (Renderer, Ada.Streams.Stream_IO.Stream (File), Summaries);
			Ada.Streams.Stream_IO.Close (File);
		end Make_Log_Index;
		procedure Make_RSS (Summaries : in Lists.Summary_Maps.Map) is
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
				I : Lists.Summary_Maps.Cursor := Summaries.Last;
			begin
				while Has_Element (I) loop
					declare
						Key : Village_Id
							renames Summaries.Constant_Reference (I).Key.all;
						Element : Lists.Village_Summary
							renames Summaries.Constant_Reference (I).Element.all;
					begin
						if Element.State = Prologue then
							Web.RSS.RSS_Item (
								Stream,
								Title => Element.Name.Constant_Reference.Element.all,
								Description => "",
								Link => "../?village=" & Key);
						end if;
					end;
					Previous (I);
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
		Village_Id : Tabula.Villages.Village_Id := Tabula.Villages.Invalid_Village_Id;
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
		elsif Village_Id = Invalid_Village_Id then
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

end Vampire.Renderers.Log;
