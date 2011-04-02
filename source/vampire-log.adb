-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Web.Producers;
with Web.RSS;
with Vampire.Configurations;
with Vampire.Forms.Full;
with Vampire.R3.Log_Index_Page;
with Vampire.R3.Village_Page;
with Vampire.Villages.Load;
package body Vampire.Log is
	use Tabula.Villages;
	use Tabula.Villages.Lists.Summary_Maps;
	use Tabula.Villages.Lists.User_Lists;
	use Villages;
	use type Ada.Strings.Unbounded.Unbounded_String;
	
	function Load_Summary (
		List : Tabula.Villages.Lists.Village_List;
		Id : Tabula.Villages.Village_Id)
		return Tabula.Villages.Lists.Village_Summary
	is
		Village : Vampire.Villages.Village_Type;
	begin
		Vampire.Villages.Load (Lists.File_Name (List, Id), Village, Info_Only => True);
		return Tabula.Villages.Lists.Summary (Type_Code, Village);
	end Load_Summary;
	
	procedure Create_Log (
		List : Tabula.Villages.Lists.Village_List;
		Id : in Tabula.Villages.Village_Id)
	is
		Form : Forms.Full.Form_Type := Forms.Full.Create;
		Template : Web.Producers.Template :=
			R3.Read (Configurations.Template_Names (Form.Template_Set).Template_Village_File_Name.all);
		Village : aliased Vampire.Villages.Village_Type;
	begin
		Vampire.Villages.Load (Lists.File_Name (List, Id), Village, Info_Only => False);
		for Day in 0 .. Village.Today loop
			declare
				Output : Ada.Streams.Stream_IO.File_Type;
			begin
				Ada.Streams.Stream_IO.Create (
					Output,
					Ada.Streams.Stream_IO.Out_File,
					Lists.HTML_File_Name (List, Id, Day));
				R3.Village_Page (
					Ada.Streams.Stream_IO.Stream(Output),
					Form,
					Template,
					Current_Directory => Configurations.Villages_HTML_Directory,
					HTML_Directory => Configurations.Villages_HTML_Directory,
					Image_Directory => Configurations.Template_Names (Form.Template_Set).Image_Directory.all,
					Style_Sheet => Configurations.Template_Names (Form.Template_Set).Style_Sheet_File_Name.all,
					Background => Configurations.Template_Names (Form.Template_Set).Background_Image_File_Name.all,
					Relative_Role_Images => Configurations.Template_Names (Form.Template_Set).Relative_Role_Image_File_Names.all,
					Cast_File_Name => Configurations.Cast_File_Name,
					Log => True,
					Village_Id => Id,
					Village => Village,
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
			Form : Forms.Full.Form_Type := Forms.Full.Create;
			File: Ada.Streams.Stream_IO.File_Type :=
				Ada.Streams.Stream_IO.Create (Name => Configurations.Villages_Index_HTML_File_Name);
		begin
			R3.Log_Index_Page (
				Ada.Streams.Stream_IO.Stream (File),
				Form,
				Configurations.Template_Names (Form.Template_Set).Template_Log_Index_File_Name.all,
				HTML_Directory => Configurations.Villages_HTML_Directory,
				Style_Sheet => Configurations.Style_Sheet_File_Name,
				Background => Configurations.Template_Names (Form.Template_Set).Background_Image_File_Name.all,
				Summaries => Summaries);
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
	
end Vampire.Log;
