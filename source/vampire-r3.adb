-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Calendar.Formatting;
with Ada.Directories;
with Ada.Exception_Identification.From_Here;
with Ada.Hierarchical_File_Names;
with Ada.Streams.Stream_IO;
with Tabula.Calendar;
with Tabula.Users;
with Vampire.Villages.Text;
package body Vampire.R3 is
	use type Ada.Calendar.Time;
	use type Tabula.Villages.Village_State;
	use type Tabula.Villages.Village_Term;
	
	function Read (
		Template_Source : in String;
		Template_Cache : in String := "")
		return Web.Producers.Template
	is
		File : Ada.Streams.Stream_IO.File_Type :=
			Ada.Streams.Stream_IO.Open (Ada.Streams.Stream_IO.In_File,
				Name => Template_Source);
	begin
		return Template : Web.Producers.Template :=
			Web.Producers.Read (
				Ada.Streams.Stream_IO.Stream (File),
				Ada.Streams.Stream_IO.Size (File),
				Parsing => False)
		do
			if Template_Cache'Length > 0 then
				if Ada.Directories.Exists (Template_Cache)
					and then Ada.Directories.Modification_Time (Template_Cache) >
					         Ada.Directories.Modification_Time (Template_Source)
				then
					-- read parsed-structure from cache file
					declare
						Cache_File : Ada.Streams.Stream_IO.File_Type;
					begin
						Ada.Streams.Stream_IO.Open (
							Cache_File,
							Ada.Streams.Stream_IO.In_File,
							Name => Template_Cache);
						Web.Producers.Read_Parsed_Information (
							Ada.Streams.Stream_IO.Stream (Cache_File),
							Template);
						Ada.Streams.Stream_IO.Close (Cache_File);
					end;
				else
					Web.Producers.Parse (Template);
					-- save parsed-structure to cache file
					declare
						Cache_File : Ada.Streams.Stream_IO.File_Type;
					begin
						Ada.Streams.Stream_IO.Create (
							Cache_File,
							Name => Template_Cache);
						Web.Producers.Write_Parsed_Information (
							Ada.Streams.Stream_IO.Stream (Cache_File),
							Template);
						Ada.Streams.Stream_IO.Close (Cache_File);
					end;
				end if;
			else
				Web.Producers.Parse (Template);
			end if;
			Ada.Streams.Stream_IO.Close(File);
		end return;
	end Read;
	
	function Day_Name (
		Day : Natural;
		Today : Natural;
		State : Tabula.Villages.Village_State)
		return String is
	begin
		if Day = 0 then
			return "プロローグ";
		elsif State >= Tabula.Villages.Epilogue and then Today = Day then
			return "エピローグ";
		else
			return Image (Day) & "日目";
		end if;
	end Day_Name;
	
	procedure Raise_Unknown_Tag (
		Tag : in String;
		File : in String := Ada.Debug.File;
		Line : in Integer := Ada.Debug.Line) is
	begin
		Ada.Exception_Identification.From_Here.Raise_Exception (
			Web.Producers.Data_Error'Identity,
			File => File,
			Line => Line,
			Message => "Invalid template """ & Tag & """");
	end Raise_Unknown_Tag;
	
	procedure Handle_User_Panel (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Template : in Web.Producers.Template;
		Form : in Forms.Root_Form_Type'Class;
		User_Id : in String;
		User_Password : in String)
	is
		Extract : constant array (Boolean) of access constant String := (
			new String'("logout"),
			new String'("login"));
		procedure Handle (
			Output : not null access Ada.Streams.Root_Stream_Type'Class;
			Tag : in String;
			Contents : in Web.Producers.Template) is
		begin
			if Tag = "action_page" then
				Forms.Write_Link (
					Output,
					Form,
					Name => "action",
					Current_Directory => ".",
					Resource => Forms.Self,
					Parameters =>
						Form.Parameters_To_Index_Page (
							User_Id => User_Id,
							User_Password => User_Password));
			elsif Tag = "administrator" then
				if User_Id = Users.Administrator then
					Web.Producers.Produce (Output, Contents, Handler => Handle'Access); -- rec
				end if;
			elsif Tag = "notadmin" then
				if User_Id /= Users.Administrator then
					Web.Producers.Produce (Output, Contents, Handler => Handle'Access); -- rec
				end if;
			elsif Tag = "id" then
				Forms.Write_In_HTML (Output, Form, User_Id);
			elsif Tag = "href_user" then
				Forms.Write_Link (
					Output,
					Form,
					Name => "href",
					Current_Directory => ".",
					Resource => Forms.Self,
					Parameters =>
						Form.Parameters_To_User_Page (
							User_Id => User_Id,
							User_Password => User_Password));
			else
				Raise_Unknown_Tag (Tag);
			end if;
		end Handle;
	begin
		Web.Producers.Produce (
			Output,
			Template,
			Extract (User_Id'Length /= 0).all,
			Handler => Handle'Access);
	end Handle_User_Panel;
	
	procedure Handle_Village_List (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Template : in Web.Producers.Template;
		Form : in Forms.Root_Form_Type'Class;
		Current_Directory : in String;
		HTML_Directory : in String;
		Summaries : in Tabula.Villages.Lists.Summary_Maps.Map;
		Log : in Boolean;
		Limits : in Natural;
		User_Id : in String;
		User_Password : in String)
	is
		Exists : Boolean := False;
	begin
		for I in Summaries.Iterate loop
			if (Summaries.Constant_Reference (I).State = Tabula.Villages.Closed) = Log then
				Exists := True;
				exit;
			end if;
		end loop;
		if Exists then
			declare
				procedure Handle (
					Output : not null access Ada.Streams.Root_Stream_Type'Class;
					Tag : in String;
					Contents : in Web.Producers.Template) is
				begin
					if Tag = "item" then
						declare
							Start : Tabula.Villages.Lists.Summary_Maps.Cursor := Summaries.First;
						begin
							if Log then
								declare
									C : Natural := Limits;
								begin
									for J in reverse Summaries.Iterate loop
										if Summaries.Constant_Reference (J).State = Tabula.Villages.Closed then
											C := C - 1;
											if C = 0 then
												Start := J;
												exit;
											end if;
										end if;
									end loop;
								end;
							end if;
							for I in Summaries.Iterate (Start, Summaries.Last) loop
								declare
									Key : Tabula.Villages.Village_Id
										renames Tabula.Villages.Lists.Summary_Maps.Key (I);
									Element : Tabula.Villages.Lists.Village_Summary
										renames Summaries.Constant_Reference (I);
								begin
									if (Element.State = Tabula.Villages.Closed) = Log then
										declare
											procedure Handle (
												Output : not null access Ada.Streams.Root_Stream_Type'Class;
												Tag : in String;
												Contents : in Web.Producers.Template) is
											begin
												if Tag = "day" then
													Forms.Write_In_HTML (
														Output,
														Form,
														Day_Name (Element.Today, Element.Today, Element.State));
												elsif Tag = "href_village" then
													Forms.Write_Link_To_Village_Page (
														Output,
														Form,
														Name => "href",
														Current_Directory => Current_Directory,
														HTML_Directory => HTML_Directory,
														Log => Log,
														Village_Id => Key,
														User_Id => User_Id,
														User_Password => User_Password);
												elsif Tag = "id" then
													Forms.Write_In_HTML (Output, Form, Key);
												elsif Tag = "name" then
													if Element.Term = Tabula.Villages.Short then
														Forms.Write_In_HTML (Output, Form, "短期 ");
													end if;
													Forms.Write_In_HTML (Output, Form, Element.Name.Constant_Reference);
												elsif Tag = "people" then
													Forms.Write_In_HTML (Output, Form, Image (Element.People.Length) & "人");
												else
													Raise_Unknown_Tag (Tag);
												end if;
											end Handle;
										begin
											Web.Producers.Produce (Output, Contents, Handler => Handle'Access);
										end;
									end if;
								end;
							end loop;
						end;
					else
						Raise_Unknown_Tag (Tag);
					end if;
				end Handle;
			begin
				Web.Producers.Produce (Output, Template, Handler => Handle'Access);
			end;
		end if;
	end Handle_Village_List;
	
	procedure Handle_Speech (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Template : in Web.Producers.Template;
		Tag : in String := "";
		Form : in Forms.Root_Form_Type'Class;
		Current_Directory : in String;
		Image_Directory : in String;
		Face_Width : in Integer;
		Face_Height : in Integer;
		Subject : in Tabula.Villages.Person_Type'Class;
		Text : in String;
		Time : in Ada.Calendar.Time;
		Filter : in String)
	is
		procedure Handle (
			Output : not null access Ada.Streams.Root_Stream_Type'Class;
			Tag : in String;
			Contents : in Web.Producers.Template) is
		begin
			if Tag = "src_image" then
				Forms.Write_Link (
					Output,
					Form,
					Name => "src",
					Current_Directory => Current_Directory,
					Resource =>
						Ada.Hierarchical_File_Names.Compose (
							Directory => Image_Directory,
							Relative_Name => Subject.Image.Constant_Reference));
			elsif Tag = "width_image" then
				if Face_Width /= 0 then
					Forms.Write_Attribute_Open (Output, "width");
					Forms.Write_In_Attribute (Output, Form, Image (Face_Width));
					Forms.Write_Attribute_Close (Output);
				end if;
			elsif Tag = "height_image" then
				if Face_Height /= 0 then
					Forms.Write_Attribute_Open (Output, "height");
					Forms.Write_In_Attribute (Output, Form, Image (Face_Height));
					Forms.Write_Attribute_Close (Output);
				end if;
			elsif Tag = "name" then
				Forms.Write_In_HTML (Output, Form, Villages.Text.Name (Subject));
			elsif Tag = "time" then
				Forms.Write_In_HTML (
					Output,
					Form,
					Ada.Calendar.Formatting.Image(Time, Time_Zone => Calendar.Time_Offset));
			elsif Tag = "text" then
				if Text'Length > Villages.Max_Length_Of_Message then
					declare
						I : Natural := Villages.Max_Length_Of_Message;
					begin
						while I >= Text'First
							and then Character'Pos (Text (I + 1)) in 16#80# .. 16#bf#
						loop
							I := I - 1;
						end loop;
						Forms.Write_In_HTML (Output, Form, Text (Text'First .. I));
						String'Write (Output, "<b>");
						Forms.Write_In_HTML (Output, Form, Text (I + 1 .. Text'Last));
						String'Write (Output, "</b>");
					end;
				else
					Forms.Write_In_HTML (Output, Form, Text);
				end if;
			elsif Tag = "class_filter" then
				Forms.Write_Attribute_Open (Output, "class");
				Forms.Write_In_Attribute (Output, Form, Filter);
				Forms.Write_Attribute_Close (Output);
			else
				Raise_Unknown_Tag (Tag);
			end if;
		end Handle;
	begin
		Web.Producers.Produce (Output, Template, Tag, Handler => Handle'Access);
	end Handle_Speech;
	
end Vampire.R3;
