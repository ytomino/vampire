-- The Village of Vampire by YT, このソースコードはNYSLです
procedure Vampire.R3.Index_Page (
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Form : in Forms.Root_Form_Type'Class;
	Template : in String;
	HTML_Directory : in String;
	Summaries : in out Tabula.Villages.Lists.Summary_Maps.Map;
	Muramura : in Natural;
	User_Id: in String;
	User_Password : in String)
is
	procedure Handle (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String;
		Template : in Web.Producers.Template) is
	begin
		if Tag = "userpanel" then
			Handle_User_Panel (
				Output,
				Template,
				Form,
				User_Id => User_Id,
				User_Password => User_Password);
		elsif Tag = "muramura" then
			for I in 1 .. Muramura loop
				Web.Producers.Produce (Output, Template);
			end loop;
		elsif Tag = "activelist" then
			Handle_Village_List (
				Output,
				Template,
				Form,
				Current_Directory => ".",
				HTML_Directory => HTML_Directory,
				Summaries => Summaries,
				Log => False,
				Limits => 9999,
				User_Id => User_Id,
				User_Password => User_Password);
		elsif Tag = "loglist" then
			Handle_Village_List (
				Output,
				Template,
				Form,
				Current_Directory => ".",
				HTML_Directory => HTML_Directory,
				Summaries => Summaries,
				Log => True,
				Limits => 10,
				User_Id => User_Id,
				User_Password => User_Password);
		else
			raise Program_Error with "Invalid template """ & Tag & """";
		end if;
	end Handle;
begin
	Web.Producers.Produce (Output, Read (Template), Handler => Handle'Access);
end Vampire.R3.Index_Page;
