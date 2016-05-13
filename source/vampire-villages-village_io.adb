-- The Village of Vampire by YT, このソースコードはNYSLです
with Ada.Strings.Unbounded;
with Tabula.Calendar.Time_IO;
with Tabula.Casts.Cast_IO;
with Tabula.Villages.Village_IO;
package body Vampire.Villages.Village_IO is
	use People;
	use Person_Records;
	use type Ada.Strings.Unbounded.Unbounded_String;
	
	procedure IO (Serializer : not null access Serialization.Serializer; Name : in String; People : in out Villages.People.Vector) is
		package Person_Records_IO is new Serialization.IO_List (
			Cursor => Villages.Person_Records.Cursor,
			Element_Type => Person_Record,
			Container_Type => Villages.Person_Records.Vector,
			Reference_Type => Villages.Person_Records.Reference_Type,
			Default => Default_Person_Record,
			Has_Element => Villages.Person_Records.Has_Element,
			Next => Villages.Person_Records.Cursor'Succ);
		package People_IO is new Serialization.IO_List (
			Cursor => Villages.People.Cursor,
			Element_Type => Person_Type,
			Container_Type => Villages.People.Vector,
			Reference_Type => Villages.People.Reference_Type,
			Default => Empty_Person,
			Has_Element => Villages.People.Has_Element,
			Next => Villages.People.Cursor'Succ);
		use Serialization;
		use Person_Records_IO;
		use Person_Role_IO;
		use Person_State_IO;
		use Requested_Role_IO;
		use People_IO;
		use Calendar.Time_IO;
		use Casts.Cast_IO;
		procedure People_Callback (Serializer : not null access Serialization.Serializer; Item : in out Person_Type) is
			procedure Person_Callback is
				procedure Person_Records_Callback (Serializer : not null access Serialization.Serializer; Item : in out Person_Record) is
					procedure Person_Record_Callback is
					begin
						IO (Serializer, "state", Item.State);
						IO (Serializer, "vote", Item.Vote, Default => Default_Person_Record.Vote);
						IO (Serializer, "provisional-vote", Item.Provisional_Vote, Default => Default_Person_Record.Provisional_Vote);
						IO (Serializer, "candidate", Item.Candidate, Default => Default_Person_Record.Candidate);
						IO (Serializer, "target", Item.Target, Default => Default_Person_Record.Target);
						IO (Serializer, "special", Item.Special, Default => Default_Person_Record.Special);
						IO (Serializer, "note", Item.Note, Default => Default_Person_Record.Note);
					end Person_Record_Callback;
				begin
					IO (Serializer, Person_Record_Callback'Access);
				end Person_Records_Callback;
			begin
				IO (Serializer, "id", Item.Id);
				IO_Partial (Serializer, Item);
				IO (Serializer, "request", Item.Request);
				IO (Serializer, "role", Item.Role);
				IO (Serializer, "ignore-request", Item.Ignore_Request, Default => Empty_Person.Ignore_Request);
				IO (Serializer, "records", Item.Records, Person_Records_Callback'Access);
				IO (Serializer, "commited", Item.Commited);
			end Person_Callback;
		begin
			IO (Serializer, Person_Callback'Access);
		end People_Callback;
	begin
		IO (Serializer, Name, People, People_Callback'Access);
	end IO;
	
	procedure IO (Serializer : not null access Serialization.Serializer; Name : in String; Messages : in out Villages.Messages.Vector) is
		use Serialization;
		use Message_Kind_IO;
		use Calendar.Time_IO;
		procedure Messages_Callback (Serializer : not null access Serialization.Serializer; Item : in out Message) is
			procedure Message_Callback is
			begin
				IO (Serializer, "day", Item.Day);
				IO (Serializer, "time", Item.Time);
				IO (Serializer, "kind", Item.Kind);
				IO (Serializer, "subject", Item.Subject, Default => Default_Message.Subject);
				IO (Serializer, "target", Item.Target, Default => Default_Message.Target);
				IO (Serializer, "text", Item.Text, Default => Default_Message.Text);
			end Message_Callback;
		begin
			IO (Serializer, Message_Callback'Access);
		end Messages_Callback;
		use Villages.Messages;
		package Messages_IO is new Serialization.IO_List (
			Cursor => Villages.Messages.Cursor,
			Element_Type => Message,
			Container_Type => Villages.Messages.Vector,
			Reference_Type => Villages.Messages.Reference_Type,
			Default => Default_Message,
			Has_Element => Villages.Messages.Has_Element,
			Next => Villages.Messages.Cursor'Succ);
		use Messages_IO;
	begin
		IO (Serializer, Name, Messages, Messages_Callback'Access);
	end IO;
	
	procedure IO (Serializer: not null access Serialization.Serializer; Village: in out Village_Type; Info_Only : in Boolean := False) is
		use Serialization;
		use Calendar.Time_IO;
		use Tabula.Villages.Village_IO.Village_State_IO;
		use Village_Time_IO;
		use Vote_IO;
		use Execution_IO;
		use Attack_IO;
		use Vampire_Action_Set_IO;
		use Servant_Knowing_IO;
		use Person_Role_IO;
		use Monster_Side_IO;
		use Role_Appearance_IO;
		use Hunter_Silver_Bullet_IO;
		use Unfortunate_IO;
		use Daytime_Preview_IO;
		use Doctor_Infected_IO;
		use Formation_IO;
		use Obsolete_Teaming_IO;
		procedure Root_Callback is
			procedure Appearance_Callback is
			begin
				for I in Village.Appearance'Range loop
					IO (Serializer, Person_Role'Image (I), Village.Appearance (I));
				end loop;
			end Appearance_Callback;
		begin
			-- inherited
			IO (Serializer, "name", Village.Name);
			IO (Serializer, "by", Village.By, Default => Ada.Strings.Unbounded.Null_Unbounded_String);
			IO (Serializer, "face-group", Village.Face_Group);
			IO (Serializer, "face-width", Village.Face_Width);
			IO (Serializer, "face-height", Village.Face_Height);
			-- additional
			IO (Serializer, "state", Village.State);
			IO (Serializer, "today", Village.Today);
			IO (Serializer, "time", Village.Time);
			IO (Serializer, "dawn", Village.Dawn);
			IO (Serializer, "day-duration", Village.Day_Duration);
			IO (Serializer, "night-duration", Village.Night_Duration);
			IO (Serializer, "vote", Village.Vote, Default => Unsigned);
			IO (Serializer, "execution", Village.Execution, Default => From_First);
			IO (Serializer, "formation", Village.Formation, Default => Public);
			IO (Serializer, "monster-side", Village.Monster_Side, Default => Fixed);
			IO (Serializer, "attack", Village.Attack, Default => Two);
			IO (Serializer, "vampire-action-set", Village.Vampire_Action_Set, Default => Gaze);
			IO (Serializer, "servant-knowing", Village.Servant_Knowing, Default => None);
			IO (Serializer, "daytime-preview", Village.Daytime_Preview, Default => Role_And_Message);
			IO (Serializer, "doctor-infected", Village.Doctor_Infected, Default => Cure);
			IO (Serializer, "hunter-silver-bullet", Village.Hunter_Silver_Bullet, Default => Target_And_Self);
			IO (Serializer, "unfortunate", Village.Unfortunate, Default => None);
			IO (Serializer, "teaming", Village.Obsolete_Teaming); -- 記録用
			if Serializer.Direction = Reading or else Village.Appearance /= Role_Appearances'(others => Random) then
				IO (Serializer, "appearance", Appearance_Callback'Access);
			end if;
			if Serializer.Direction = Reading or else Village.Execution = Dummy_Killed_And_From_First then
				IO (Serializer, "dummy-role", Village.Dummy_Role);
			end if;
			IO (Serializer, "people", Village.People);
			if Serializer.Direction = Reading or else not Village.Escaped_People.Is_Empty then
				IO (Serializer, "escaped-people", Village.Escaped_People);
			end if;
			if not Info_Only then
				IO (Serializer, "messages", Village.Messages);
			end if;
		end Root_Callback;
	begin
		IO (Serializer, Root_Callback'Access);
	end IO;
	
end Vampire.Villages.Village_IO;
