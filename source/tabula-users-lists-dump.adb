-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Calendar.Formatting;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
procedure Tabula.Users.Lists.Dump (
	Users_Directory : in not null Static_String_Access;
	Users_Log_File_Name : in not null Static_String_Access)
is
	use type Ada.Strings.Unbounded.Unbounded_String;
	Input_File_Name : aliased Ada.Strings.Unbounded.Unbounded_String :=
		+Users_Log_File_Name.all;
	type Item_Type is record
		Id : aliased Ada.Strings.Unbounded.Unbounded_String;
		Remote_Addr : aliased Ada.Strings.Unbounded.Unbounded_String;
		Remote_Host : aliased Ada.Strings.Unbounded.Unbounded_String;
		Time : Ada.Calendar.Time;
	end record;
	package Item_Lists is new Ada.Containers.Doubly_Linked_Lists (Item_Type);
	use Item_Lists;
	Items : aliased Item_Lists.List;
begin
	if Ada.Command_Line.Argument_Count > 0 then
		Input_File_Name := +Ada.Command_Line.Argument (1);
	end if;
	declare
		use Ada.Text_IO;
		procedure Process (
			Id : in String;
			Remote_Addr : in String;
			Remote_Host : in String;
			Time : in Ada.Calendar.Time) is
		begin
			Put (Id);
			Put (',');
			Put (Remote_Addr);
			Put (',');
			Put (Remote_Host);
			Put (',');
			Put (Ada.Calendar.Formatting.Image (Time));
			New_Line;
			Append (Items, (
				Id => +Id,
				Remote_Addr => +Remote_Addr,
				Remote_Host => +Remote_Host,
				Time => Time));
		end Process;
		List : User_List := Create (
			Directory => Users_Directory,
			Log_File_Name => Input_File_Name.Constant_Reference.Element);
	begin
		Iterate_Log (List, Process'Access);
		New_Line;
	end;
	if Items.Length >= 2 then
		for I in Items.Iterate (Items.First, Previous (Items.Last)) loop
			declare
				I_Ref : constant Item_Lists.Constant_Reference_Type := Items.Constant_Reference (I);
			begin
				for J in Items.Iterate (Next (I), Items.Last) loop
					declare
						use Ada.Text_IO;
						J_Ref : constant Item_Lists.Constant_Reference_Type := Items.Constant_Reference (J);
					begin
						if I_Ref.Remote_Addr = J_Ref.Remote_Addr
							or else (
								I_Ref.Remote_Host = J_Ref.Remote_Host
								and then not J_Ref.Remote_Host.Is_Null)
						then
							Put (I_Ref.Id.Constant_Reference);
							Put (',');
							Put (I_Ref.Remote_Addr.Constant_Reference);
							Put (',');
							Put (I_Ref.Remote_Host.Constant_Reference);
							Put (',');
							Put (Ada.Calendar.Formatting.Image (I_Ref.Time));
							New_Line;
							Put (J_Ref.Id.Constant_Reference);
							Put (',');
							Put (J_Ref.Remote_Addr.Constant_Reference);
							Put (',');
							Put (J_Ref.Remote_Host.Constant_Reference);
							Put (',');
							Put (Ada.Calendar.Formatting.Image (J_Ref.Time));
							New_Line;
						end if;
					end;
				end loop;
			end;
		end loop;
	end if;
end Tabula.Users.Lists.Dump;
