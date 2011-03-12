-- The Village of Vampire by YT, このソースコードはNYSLです
-- ユーザーログ表示ツールです、CGIとして公開しないでください
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Calendar.Formatting;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with Tabula.Configurations;
procedure Tabula.Users.Lists.Dump is
	use type Ada.Strings.Unbounded.Unbounded_String;
	use type Ada.Calendar.Time;
	Users_Log_File_Name : aliased Ada.Strings.Unbounded.Unbounded_String :=
		+Tabula.Configurations.Users_Log_File_Name;
	type Item_Type is record
		Id : aliased Ada.Strings.Unbounded.Unbounded_String;
		Remote_Addr : aliased Ada.Strings.Unbounded.Unbounded_String;
		Remote_Host : aliased Ada.Strings.Unbounded.Unbounded_String;
		Time : Ada.Calendar.Time;
	end record;
	package Item_Lists is new Ada.Containers.Doubly_Linked_Lists (Item_Type);
	Items : aliased Item_Lists.List;
begin
	if Ada.Command_Line.Argument_Count > 0 then
		Users_Log_File_Name := +Ada.Command_Line.Argument (1);
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
			Item_Lists.Append (Items, (
				Id => +Id,
				Remote_Addr => +Remote_Addr,
				Remote_Host => +Remote_Host,
				Time => Time));
		end Process;
		List : Users_List := Create (
			Directory => Tabula.Configurations.Users_Directory'Access,
			Log_File_Name => Static_String_Access (Users_Log_File_Name.Constant_Reference.Element));
	begin
		Iterate_Log (List, Process'Access);
		New_Line;
	end;
	declare
		I : Item_Lists.Cursor := Items.First;
	begin
		while Item_Lists.Has_Element (I) loop
			declare
				pragma Warnings (Off);
				I_Ref : constant Item_Lists.Constant_Reference_Type := Items.Constant_Reference (I);
				pragma Warnings (On);
				J : Item_Lists.Cursor := Item_Lists.Next (I);
			begin
				while Item_Lists.Has_Element(J) loop
					declare
						use Ada.Text_IO;
						pragma Warnings (Off);
						J_Ref : constant Item_Lists.Constant_Reference_Type := Items.Constant_Reference (J);
						pragma Warnings (On);
					begin
						if I_Ref.Element.Remote_Addr = J_Ref.Element.Remote_Addr 
							or else (
								I_Ref.Element.Remote_Host = J_Ref.Element.Remote_Host
								and then J_Ref.Element.Remote_Host /= "")
						then
							Put(I_Ref.Element.Id.Constant_Reference.Element.all);
							Put(',');
							Put(I_Ref.Element.Remote_Addr.Constant_Reference.Element.all);
							Put(',');
							Put(I_Ref.Element.Remote_Host.Constant_Reference.Element.all);
							Put(',');
							Put(Ada.Calendar.Formatting.Image (I_Ref.Element.Time));
							New_Line;
							Put(J_Ref.Element.Id.Constant_Reference.Element.all);
							Put(',');
							Put(J_Ref.Element.Remote_Addr.Constant_Reference.Element.all);
							Put(',');
							Put(J_Ref.Element.Remote_Host.Constant_Reference.Element.all);
							Put(',');
							Put(Ada.Calendar.Formatting.Image (J_Ref.Element.Time));
							New_Line;
						end if;
					end;
					Item_Lists.Next (J);
				end loop;
			end;
			Item_Lists.Next (I);
		end loop;
	end;
end Tabula.Users.Lists.Dump;
