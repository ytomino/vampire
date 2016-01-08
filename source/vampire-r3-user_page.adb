-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Strings.Unbounded;
procedure Vampire.R3.User_Page (
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Form : in Forms.Root_Form_Type'Class;
	Template : in String;
	Summaries : in Tabula.Villages.Lists.Summary_Maps.Map;
	User_Id : in String;
	User_Password : in String;
	User_Info : in Users.User_Info)
is
	use Tabula.Villages.Lists.Summary_Maps;
	use Tabula.Villages.Lists.User_Lists;
	use type Ada.Strings.Unbounded.Unbounded_String;
	use type Tabula.Villages.Village_State;
	-- ユーザーの参加状況
	Joined : aliased Ada.Strings.Unbounded.Unbounded_String;
	Created : aliased Ada.Strings.Unbounded.Unbounded_String;
	procedure Handle(
		Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String;
		Template : in Web.Producers.Template) is
	begin
		if Tag = "action_page" then
			Forms.Write_Attribute_Name (Output, "action");
			Forms.Write_Link (
				Output,
				Form,
				Current_Directory => ".",
				Resource => Forms.Self,
				Parameters => Form.Parameters_To_User_Page (
					User_Id => User_Id,
					User_Password => User_Password));
		elsif Tag = "userpanel" then
			Handle_User_Panel (
				Output,
				Template,
				Form,
				User_Id => User_Id,
				User_Password => User_Password);
		elsif Tag = "id" then
			Forms.Write_In_HTML (Output, Form, User_Id);
		elsif Tag = "joined" then
			if Joined /= Ada.Strings.Unbounded.Null_Unbounded_String then
				Web.Producers.Produce(Output, Template, Handler => Handle'Access);
			end if;
		elsif Tag = "nojoined" then
			if Joined = Ada.Strings.Unbounded.Null_Unbounded_String then
				Web.Producers.Produce(Output, Template);
			end if;
		elsif Tag = "created" then
			if Created /= Ada.Strings.Unbounded.Null_Unbounded_String then
				Web.Producers.Produce(Output, Template, Handler => Handle'Access);
			end if;
		elsif Tag = "creatable" then
			if Joined = Ada.Strings.Unbounded.Null_Unbounded_String
				and then Created = Ada.Strings.Unbounded.Null_Unbounded_String
			then
				Web.Producers.Produce(Output, Template, Handler => Handle'Access);
			end if;
		elsif Tag = "activevillage" then
			Forms.Write_In_HTML (Output, Form, Joined.Constant_Reference);
		elsif Tag = "createdvillage" then
			Forms.Write_In_HTML (Output, Form, Created.Constant_Reference);
		elsif Tag = "href_index" then
			Forms.Write_Attribute_Name (Output, "href");
			Forms.Write_Link (
				Output,
				Form,
				Current_Directory => ".",
				Resource => Forms.Self,
				Parameters => Form.Parameters_To_Index_Page (
					User_Id => User_Id,
					User_Password => User_Password));
		else
			raise Program_Error with "Invalid template """ & Tag & """";
		end if;
	end Handle;
begin
	for I in Summaries.Iterate loop
		declare
			V : Tabula.Villages.Lists.Village_Summary
				renames Summaries.Constant_Reference (I);
		begin
			if V.State < Tabula.Villages.Epilogue then
				for J in V.People.Iterate loop
					if V.People.Constant_Reference (J) = User_Id then
						if Joined /= Ada.Strings.Unbounded.Null_Unbounded_String then
							Ada.Strings.Unbounded.Append(Joined, "、");
						end if;
						Ada.Strings.Unbounded.Append(Joined, V.Name);
					end if;
				end loop;
				if V.By = User_Id then
					Created := V.Name;
				end if;
			end if;
		end;
	end loop;
	Web.Producers.Produce (Output, Read (Template), Handler => Handle'Access);
end Vampire.R3.User_Page;
