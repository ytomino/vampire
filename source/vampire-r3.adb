-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Calendar;
with Ada.Directories;
with Ada.Streams.Stream_IO;
with Tabula.Users;
package body Vampire.R3 is
	use Tabula.Villages.Lists.Summary_Maps;
	use type Ada.Calendar.Time;
	use type Tabula.Villages.Village_State;
	use type Tabula.Villages.Village_Term;
	
	procedure Produce (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Template_Source : in String;
		Template_Cache : in String := "";
		Handler : not null access procedure (
			Output : not null access Ada.Streams.Root_Stream_Type'Class;
			Tag : in String;
			Contents : Web.Producers.Template))
	is
		File : Ada.Streams.Stream_IO.File_Type :=
			Ada.Streams.Stream_IO.Open (Ada.Streams.Stream_IO.In_File, Template_Source);
		Template : Web.Producers.Template := Web.Producers.Read (
			Ada.Streams.Stream_IO.Stream (File),
			Ada.Streams.Stream_IO.Size(File),
			Parsing => False);
	begin
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
						Ada.Streams.Stream_IO.Out_File,
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
		Web.Producers.Produce (Output, Template, Handler => Handler);
		Ada.Streams.Stream_IO.Close(File);
	end Produce;
	
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
	
	function Name (Person : Vampire.Villages.Person_Type) return String is
	begin
		return Person.Work.Constant_Reference.Element.all &
			Person.Name.Constant_Reference.Element.all;
	end Name;
	
	procedure Handle_User_Panel (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Template : in Web.Producers.Template;
		Form : in Forms.Root_Form_Type'Class;
		User_Id : in String;
		User_Password : in String)
	is
		procedure Handle (
			Output : not null access Ada.Streams.Root_Stream_Type'Class;
			Tag : in String;
			Template : in Web.Producers.Template) is
		begin
			if Tag = "action_page" then
				String'Write (Output, "action=");
				Forms.Write_Link (
					Output,
					Form,
					Current_Directory => ".",
					Resource => Forms.Self,
					Parameters => Form.Parameters_To_Index_Page (
						User_Id => User_Id,
						User_Password => User_Password));
			elsif Tag = "administrator" then
				if User_Id = Users.Administrator then
					Web.Producers.Produce (Output, Template, Handler => Handle'Access);
				end if;
			elsif Tag = "not_admin" then
				if User_Id /= Users.Administrator then
					Web.Producers.Produce (Output, Template, Handler => Handle'Access);
				end if;
			elsif Tag = "id" then
				Forms.Write_In_HTML (Output, Form, User_Id);
			elsif Tag = "href_user" then
				String'Write (Output, "href=");
				Forms.Write_Link (
					Output,
					Form,
					Current_Directory => ".",
					Resource => Forms.Self,
					Parameters => Form.Parameters_To_User_Page (
						User_Id => User_Id,
						User_Password => User_Password));
			else
				raise Program_Error with "Invalid template """ & Tag & """";
			end if;
		end Handle;
		Extract : constant array (Boolean) of access constant String := (
			new String'("logoff"),
			new String'("logon"));
	begin
		Web.Producers.Produce (Output, Template, Extract (User_Id /= "").all, Handler => Handle'Access);
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
		procedure Handle (
			Output : not null access Ada.Streams.Root_Stream_Type'Class;
			Tag : in String;
			Template : in Web.Producers.Template) is
		begin
			if Tag = "item" then
				declare
					I : Tabula.Villages.Lists.Summary_Maps.Cursor := Summaries.First;
				begin
					if Log then
						declare
							C : Natural := Limits;
							J : Tabula.Villages.Lists.Summary_Maps.Cursor := Summaries.Last;
						begin
							while Has_Element (J) loop
								if Summaries.Constant_Reference (J).Element.State = Tabula.Villages.Closed then
									C := C - 1;
									if C = 0 then
										I := J;
										exit;
									end if;
								end if;
								Previous (J);
							end loop;
						end;
					end if;
					while Has_Element (I) loop
						declare
							Key : Tabula.Villages.Village_Id
								renames Summaries.Constant_Reference (I).Key.all;
							Element : Tabula.Villages.Lists.Village_Summary
								renames Summaries.Constant_Reference (I).Element.all;
							procedure Handle_Item (Output : not null access Ada.Streams.Root_Stream_Type'Class;
								Tag : in String; Template : in Web.Producers.Template) is
							begin
								if Tag = "day" then
									Forms.Write_In_HTML (
										Output,
										Form,
										Day_Name (
											Element.Today,
											Element.Today,
											Element.State));
								elsif Tag = "href_village" then
									String'Write (Output, "href=");
									Forms.Write_Link_To_Village_Page (
										Output,
										Form,
										Current_Directory => Current_Directory,
										HTML_Directory => HTML_Directory,
										Log => Log,
										Village_Id => Key,
										User_Id => User_Id,
										User_Password => User_Password);
								elsif Tag = "id" then
									Forms.Write_In_HTML (
										Output,
										Form,
										Key);
								elsif Tag = "name" then
									if Element.Term = Tabula.Villages.Short then
										Forms.Write_In_HTML (
											Output,
											Form,
											"短期 ");
									end if;
									Forms.Write_In_HTML (
										Output,
										Form,
										Element.Name.Constant_Reference.Element.all);
								elsif Tag = "people" then
									Forms.Write_In_HTML (
										Output,
										Form,
										Image (Element.People.Length) & "人");
								else
									raise Program_Error with "Invalid template """ & Tag & """";
								end if;
							end Handle_Item;
						begin
							if (Element.State = Tabula.Villages.Closed) = Log then
								Web.Producers.Produce (Output, Template, Handler => Handle_Item'Access);
							end if;
						end;
						Next (I);
					end loop;
				end;
			else
				raise Program_Error with "Invalid template """ & Tag & """";
			end if;
		end Handle;
		Exists : Boolean := False;
	begin
		declare
			I : Tabula.Villages.Lists.Summary_Maps.Cursor := Summaries.First;
		begin
			while Has_Element (I) loop
				if (Summaries.Constant_Reference (I).Element.State = Tabula.Villages.Closed) = Log then
					Exists := True;
					exit;
				end if;
				Next (I);
			end loop;
		end;
		if Exists then
			Web.Producers.Produce (Output, Template, Handler => Handle'Access);
		end if;
	end Handle_Village_List;
	
end Vampire.R3;
