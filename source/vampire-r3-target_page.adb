-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Strings.Unbounded;
with Vampire.Villages.Text;
procedure Vampire.R3.Target_Page (
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Form : in Forms.Root_Form_Type'Class;
	Template : in String;
	Village_Id : in Tabula.Villages.Village_Id;
	Village : in Villages.Village_Type;
	Player : in Tabula.Villages.Person_Index;
	Target : in Tabula.Villages.Person_Index;
	User_Id : in String;
	User_Password : in String)
is
	use type Ada.Strings.Unbounded.Unbounded_String;
	Person : Villages.Person_Type
		renames Village.People.Constant_Reference(Player).Element.all;
	Target_Person : Villages.Person_Type
		renames Village.People.Constant_Reference(Target).Element.all;
	procedure Handle (
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String;
		Template : in Web.Producers.Template) is
	begin
		if Tag = "action_cgi" then
			Forms.Write_Attribute_Name (Output, "action");
			Forms.Write_Link (
				Output,
				Form,
				Current_Directory => ".",
				Resource => Forms.Self);
		elsif Tag = "parameters" then
			Web.Write_Query_In_HTML (
				Output,
				Form.HTML_Version,
				Form.Parameters_To_Village_Page (
					Village_Id => Village_Id,
					User_Id => User_Id,
					User_Password => User_Password));
		elsif Tag = "action_page" then
			Forms.Write_Attribute_Name (Output, "action");
			Forms.Write_Link (
				Output,
				Form,
				Current_Directory => ".",
				Resource => Forms.Self,
				Parameters => Form.Parameters_To_Village_Page (
					Village_Id => Village_Id,
					User_Id => User_Id,
					User_Password => User_Password));
		elsif Tag = "villagename" then
			Forms.Write_In_HTML (
				Output,
				Form,
				Village.Name.Constant_Reference.Element.all);
		elsif Tag = "message" then
			case Person.Role is
				when Villages.Doctor =>
					Forms.Write_In_HTML (
						Output,
						Form,
						Villages.Text.Name (Target_Person) & "を診察しますか？");
				when Villages.Detective =>
					if Target_Person.Records.Constant_Reference(Village.Today).Element.Note = "" then
						Forms.Write_In_HTML (
							Output,
							Form,
							"遺言を読み解くにはもう少しかかりそうですが、現時点で");
					end if;
					Forms.Write_In_HTML (
						Output,
						Form,
						Villages.Text.Name (Target_Person) & "を調査しますか？");
				when others =>
					raise Program_Error;
			end case;
		elsif Tag = "value_submit" then
			Forms.Write_Attribute_Name (Output, "value");
			Forms.Write_Attribute_Open (Output);
			case Person.Role is
				when Villages.Doctor =>
					Forms.Write_In_Attribute (Output, Form, "診察");
				when Villages.Detective =>
					Forms.Write_In_Attribute (Output, Form, "調査");
				when others =>
					raise Program_Error;
			end case;
			Forms.Write_Attribute_Close (Output);
		elsif Tag = "value_target" then
			Forms.Write_Attribute_Name (Output, "value");
			Forms.Write_Attribute_Open (Output);
			Forms.Write_In_Attribute (Output, Form, Image (Target));
			Forms.Write_Attribute_Close (Output);
		else
			raise Program_Error with "Invalid template """ & Tag & """";
		end if;
	end Handle;
begin
	Web.Producers.Produce (Output, Read (Template), Handler => Handle'Access);
end Vampire.R3.Target_Page;