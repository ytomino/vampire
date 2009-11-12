-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Ada.Strings.Unbounded;
with Web.RSS;
with Tabula.Configurations;
with Tabula.Configurations.Templates;
with Tabula.Renderers.Log;
with Tabula.String_Lists;
with Tabula.Villages.Lists;
with Tabula.Vampires.Villages.Load;
package body Tabula.Villages.Lists.Managing is
	use type Ada.Streams.Stream_Element_Offset;
	use type Ada.Strings.Unbounded.Unbounded_String;
	use type Tabula.Villages.Lists.Village_List_Item;
	use Tabula.String_Lists;
	use Tabula.Villages.Lists.Village_Lists;
	
	function "+" (S : Ada.Strings.Unbounded.Unbounded_String) return String renames Ada.Strings.Unbounded.To_String;
	-- function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String renames Ada.Strings.Unbounded.To_Unbounded_String;

	function To_String(X : Integer) return String is
		Result : String := Integer'Image(X);
	begin
		if Result (Result'First) = ' ' then
			return Result (Result'First + 1 .. Result'Last);
		else
			return Result;
		end if;
	end To_String;
	
	procedure Refresh_Village_List is
	begin
		Ada.Directories.Delete_File(Configurations.Village_List_Cache_File_Name);
		declare
			List : aliased constant Village_Lists.Vector := Village_List;
		begin
			Make (List'Access); -- ログのindex作成
			Make_RSS (List'Access);
		end;
	exception
		when Ada.Directories.Name_Error => null;
	end Refresh_Village_List;
	
	procedure Clear_Village_List is
		Search : Ada.Directories.Search_Type;
		File : Ada.Directories.Directory_Entry_Type;
	begin
		Ada.Directories.Start_Search(Search, Configurations.Villages_HTML_Directory, "*.html");
		while Ada.Directories.More_Entries(Search) loop
			Ada.Directories.Get_Next_Entry(Search, File);
			declare
				File_Name : String renames Ada.Directories.Full_Name(File);
			begin
				Ada.Directories.Delete_File(File_Name);
			end;
		end loop;
		Ada.Directories.End_Search(Search);
		Refresh_Village_List;
	end Clear_Village_List;
	
	function New_Village_Id return Villages.Lists.Village_Id is
		Search : Ada.Directories.Search_Type;
		File : Ada.Directories.Directory_Entry_Type;
		Result : Integer := 0;
	begin
		Ada.Directories.Start_Search(Search, Configurations.Villages_Data_Directory, "????");
		while Ada.Directories.More_Entries(Search) loop
			Ada.Directories.Get_Next_Entry(Search, File);
			declare
				File_Name : String renames Ada.Directories.Simple_Name(File);
			begin
				declare
					Num : constant Integer := Integer'Value(File_Name);
				begin
					if Num >= Result then
						Result := Num + 1;
					end if;
				end;
			exception
				when Constraint_Error => null;
			end;
		end loop;
		Ada.Directories.End_Search(Search);
		declare
			Image : String := Integer'Image(Result);
		begin
			if Image (Image'First) = ' ' then
				Image (Image'First) := '0';
			end if;
			return (1 .. 4 - Image'Length => '0') & Image;
		end;
	end New_Village_Id;

	function Village_List return Villages.Lists.Village_Lists.Vector is
		package Sorting is new Villages.Lists.Village_Lists.Generic_Sorting;
		Search : Ada.Directories.Search_Type;
		File : Ada.Directories.Directory_Entry_Type;
		Result : Villages.Lists.Village_Lists.Vector;
	begin
		if Ada.Directories.Exists(Configurations.Village_List_Cache_File_Name) then
			declare
				Cache_File : Ada.Streams.Stream_IO.File_Type;
			begin
				Ada.Streams.Stream_IO.Open(Cache_File, Ada.Streams.Stream_IO.In_File, Configurations.Village_List_Cache_File_Name);
				begin
					Villages.Lists.Village_Lists.Vector'Read(Ada.Streams.Stream_IO.Stream(Cache_File), Result);
					Ada.Streams.Stream_IO.Close(Cache_File);
					return Result;
				exception
					when others =>
						Ada.Streams.Stream_IO.Close(Cache_File);
						raise;
				end;
			end;
		end if;
		Ada.Directories.Start_Search(Search, Configurations.Villages_Data_Directory, "????");
		while Ada.Directories.More_Entries(Search) loop
			Ada.Directories.Get_Next_Entry(Search, File);
			declare
				Num : String renames Ada.Directories.Simple_Name(File);
			begin
				if Num(Num'First) in '0' .. '9' then
					declare
						Village : Vampires.Villages.Village_Type;
						Village_Id : String renames Ada.Directories.Simple_Name(File);
						Id_List : String_Lists.List;
					begin
						Vampires.Villages.Load(Village_Id, Village, Info_Only => True);
						for I in Village.People.First_Index .. Village.People.Last_Index loop
							Append(Id_List, +Village.People.Constant_Reference(I).Element.Id);
						end loop;
						Append(Result, Villages.Lists.Village_List_Item'(
							Id => Num, 
							Name => Village.Name, 
							By => Village.By,
							Day_Duration => Village.Day_Duration,
							People => Id_List, 
							Today => Village.Today,
							State => Village.State));
						case Village.State is
							when Villages.Prologue | Villages.Opened | Villages.Epilogue => null;
							when Villages.Closed =>
								declare
									Log : String renames Ada.Directories.Compose(Configurations.Villages_HTML_Directory, Num & "-0.html");
								begin
									if not Ada.Directories.Exists(Log) then
										Vampires.Villages.Load(Village_Id, Village, Info_Only => False);
										for Day in 0 .. Village.Today loop
											declare
												Renderer : Tabula.Renderers.Log.Renderer(Configurations.Templates.Configuration);
												Log_File_Name : String renames Ada.Directories.Compose(Configurations.Villages_HTML_Directory,
													Num & "-" & To_String(Day) & ".html");
												Output : Ada.Streams.Stream_IO.File_Type;
											begin
												Ada.Streams.Stream_IO.Create(Output, Ada.Streams.Stream_IO.Out_File, Log_File_Name);
												Renderers.Log.Village_Page(
													Renderer,
													Ada.Streams.Stream_IO.Stream(Output),
													Num,
													Village, 
													Day => Day, 
													User_Id => "",
													User_Password => "");
												Ada.Streams.Stream_IO.Close(Output);
											end;
										end loop;
									end if;
								end;
						end case;
					end;
				end if;
			end;
		end loop;
		Ada.Directories.End_Search(Search);
		Sorting.Sort(Result);
		declare
			File: Ada.Streams.Stream_IO.File_Type;
		begin
			Ada.Streams.Stream_IO.Create(File, Ada.Streams.Stream_IO.Out_File, Configurations.Village_List_Cache_File_Name);
			Villages.Lists.Village_Lists.Vector'Write(Ada.Streams.Stream_IO.Stream(File), Result);
			Ada.Streams.Stream_IO.Close(File);
		exception
			when others =>
				Ada.Streams.Stream_IO.Close(File);
				raise;
		end;
		return Result;
	end Village_List;
	
	procedure Make (List : not null access constant Lists.Village_Lists.Vector) is
		Renderer : Renderers.Log.Renderer := Renderers.Log.Renderer'(Configuration => Configurations.Templates.Configuration);
		File: Ada.Streams.Stream_IO.File_Type;
	begin
		Ada.Streams.Stream_IO.Create(File, Name => Configurations.List_HTML_File_Name);
		begin
			Renderer.List_Page(Ada.Streams.Stream_IO.Stream(File), List.all);
		exception
			when others =>
				Ada.Streams.Stream_IO.Close(File);
				raise;
		end;
		Ada.Streams.Stream_IO.Close(File);
	end Make;
	
	procedure Make_RSS (List : not null access constant Lists.Village_Lists.Vector) is
		File: Ada.Streams.Stream_IO.File_Type;
	begin
		Ada.Streams.Stream_IO.Create (File, Ada.Streams.Stream_IO.Out_File, Configurations.List_RSS_File_Name);
		declare
			Stream : not null access Ada.Streams.Root_Stream_Type'Class := Ada.Streams.Stream_IO.Stream (File);
		begin
			Web.RSS.RSS_Start (Stream,
				Title => "参加募集中の村 - The Village of Vampire",
				Description => "",
				Link => "../");
			for I in reverse List.First_Index .. List.Last_Index loop
				declare
					Item : Village_List_Item renames List.Constant_Reference (I).Element.all;
				begin
					if Item.State = Prologue then
						Web.RSS.RSS_Item (Stream,
							Title => +Item.Name,
							Description => "",
							Link => "../?village=" & Item.Id);
					end if;
				end;
			end loop;
			Web.RSS.RSS_End (Stream);
		exception
			when others =>
				Ada.Streams.Stream_IO.Close (File);
				raise;
		end;
		Ada.Streams.Stream_IO.Close (File);
	end Make_RSS;
	
	function Exists(Id : Village_Id) return Boolean is
	begin
		return Ada.Directories.Exists(Ada.Directories.Compose(Configurations.Villages_Data_Directory, Id));
	end Exists;
	
	function Short_Term_Village_Blocking return Boolean is
	begin
		return Ada.Directories.Exists(Configurations.Short_Term_Village_Blocking_File_Name);
	end Short_Term_Village_Blocking;

end Tabula.Villages.Lists.Managing;
