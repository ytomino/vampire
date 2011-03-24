-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Strings.Unbounded;
procedure Vampire.Renderers.Target_Page (
	Object : in Renderer'Class;
	Output : not null access Ada.Streams.Root_Stream_Type'Class;
	Village_Id : in Tabula.Villages.Village_Id;
	Village : in Vampire.Villages.Village_Type;
	Player : in Natural;
	Target : in Natural;
	User_Id : in String;
	User_Password : in String)
is
	use type Ada.Strings.Unbounded.Unbounded_String;
	Person : Vampire.Villages.Person_Type renames Village.People.Constant_Reference(Player).Element.all;
	Target_Person : Vampire.Villages.Person_Type renames Village.People.Constant_Reference(Target).Element.all;
	procedure Handle(Output : not null access Ada.Streams.Root_Stream_Type'Class;
		Tag : in String; Template : in Web.Producers.Template) is
	begin
		if Tag = "message" then
			case Person.Role is
				when Vampire.Villages.Doctor =>
					Write(Output, Renderers.Name(Target_Person) & "を診察しますか？");
				when Vampire.Villages.Detective =>
					if Target_Person.Records.Constant_Reference(Village.Today).Element.Note = "" then
						Write(Output, "遺言を読み解くにはもう少しかかりそうですが、現時点で");
					end if;
					Write(Output, Renderers.Name(Target_Person) & "を調査しますか？");
				when others =>
					raise Program_Error;
			end case;
		elsif Tag = "button" then
			case Person.Role is
				when Vampire.Villages.Doctor => Write(Output, "診察");
				when Vampire.Villages.Detective => Write(Output, "調査");
				when others => raise Program_Error;
			end case;
		elsif Tag = "target" then
			Write(Output, '"');
			Write(Output, To_String(Target));
			Write(Output, '"');
		else
			Handle_Villages(Output, Tag, Template, Object,
				Village_Id, Village, Village.Today, User_Id => User_Id, User_Password => User_Password);
		end if;
	end Handle;
begin
	Produce (Object, Output, Object.Configuration.Template_Target_File_Name.all, Handle'Access);
end Vampire.Renderers.Target_Page;
