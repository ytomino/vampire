-- The Village of Vampire by YT, このソースコードはNYSLです
with Tabula.Calendar.Time_IO;
package body Tabula.Villages.Village_IO is
	
	use type Ada.Strings.Unbounded.Unbounded_String;
	
	procedure IO(Serializer : in out DYAYaml.Serializer; Name : String; People : in out Person_Array_Access) is
		package Person_Records_IO is new DYAYaml.IO_Array(Natural, Person_Record, Person_Record_Array, Person_Record_Array_Access, Default_Person_Record);
		package People_IO is new DYAYaml.IO_Array(Natural, Person_Type, Person_Array, Person_Array_Access, Default_Person);
		use DYAYaml;
		use Person_Records_IO;
		use Person_Role_IO;
		use Person_State_IO;
		use Requested_Role_IO;
		use Sex_Kind_IO;
		use People_IO;
		use Tabula.Calendar.Time_IO;
		procedure People_Callback(Item : in out Person_Type) is
			procedure Person_Callback is
				procedure Person_Records_Callback(Item : in out Person_Record) is
					procedure Person_Record_Callback is
					begin
						IO(Serializer, "state", Item.State);
						IO(Serializer, "vote", Item.Vote, Default => Default_Person_Record.Vote);
						IO(Serializer, "target", Item.Target, Default => Default_Person_Record.Target);
						IO(Serializer, "special", Item.Special, Default => Default_Person_Record.Special);
						IO(Serializer, "note", Item.Note, Default => Default_Person_Record.Note);
					end Person_Record_Callback;
				begin
					DYAYaml.IO(Serializer, Person_Record_Callback'Access);
				end Person_Records_Callback;
			begin
				IO(Serializer, "id", Item.Id);
				IO(Serializer, "name", Item.Name);
				IO(Serializer, "work", Item.Work);
				IO(Serializer, "image", Item.Image);
				IO(Serializer, "sex", Item.Sex);
				IO(Serializer, "group", Item.Group);
				IO(Serializer, "request", Item.Request);
				IO(Serializer, "role", Item.Role);
				IO(Serializer, "ignore-request", Item.Ignore_Request, Default => Default_Person.Ignore_Request);
				IO(Serializer, "records", Item.Records, Person_Records_Callback'Access);
				IO(Serializer, "commited", Item.Commited);
			end Person_Callback;
		begin
			IO(Serializer, Person_Callback'Access);
		end People_Callback;
	begin
		IO(Serializer, Name, People, People_Callback'Access);
	end IO;
	
	procedure IO(Serializer : in out DYAYaml.Serializer; Name : String; Messages : in out Villages.Messages.Vector) is
		use DYAYaml;
		use Tabula.Calendar.Time_IO;
		use Message_Kind_IO;
		procedure Messages_Callback(Position : Villages.Messages.Cursor) is
			procedure Process(Item : in out Message) is
				procedure Message_Callback is
				begin
					IO(Serializer, "day", Item.Day);
					IO(Serializer, "time", Item.Time);
					IO(Serializer, "kind", Item.Kind);
					IO(Serializer, "subject", Item.Subject, Default => Default_Message.Subject);
					IO(Serializer, "target", Item.Target, Default => Default_Message.Target);
					IO(Serializer, "text", Item.Text, Default => Default_Message.Text);
				end Message_Callback;
			begin
				DYAYaml.IO(Serializer, Message_Callback'Access);
			end Process;
		begin
			Villages.Messages.Update_Element(Messages, Position, Process'Access);
		end Messages_Callback;
		use Villages.Messages;
		package Messages_IO is new DYAYaml.IO_List(Villages.Messages.Vector, Villages.Messages.Cursor, Message, Default_Message);
	begin
		Messages_IO.IO(Serializer, Name, Messages, Messages_Callback'Access);
	end IO;
	
	procedure IO(Serializer: in out DYAYaml.Serializer; Village: in out Village_Type; Info_Only : Boolean := False) is
		use DYAYaml;
		use Tabula.Calendar.Time_IO;
		use Village_State_IO;
		use Attack_IO;
		use Servant_Knowing_IO;
		use Person_Role_IO;
		use Monster_Side_IO;
		use Teaming_IO;
		use Village_Time_IO;
		use Role_Appearance_IO;
		use Hunter_Silver_Bullet_IO;
		use Unfortunate_IO;
		use Daytime_Preview_IO;
		use Doctor_Infected_IO;
		procedure Root_Callback is
			procedure Appearance_Callback is
			begin
				for I in Village.Appearance'Range loop
					IO(Serializer, Person_Role'Image(I), Village.Appearance(I));
				end loop;
			end Appearance_Callback;
		begin
			IO(Serializer, "name", Village.Name);
			IO(Serializer, "by", Village.By, Default => Ada.Strings.Unbounded.Null_Unbounded_String);
			IO(Serializer, "state", Village.State);
			IO(Serializer, "today", Village.Today);
			IO(Serializer, "time", Village.Time);
			IO(Serializer, "dawn", Village.Dawn);
			IO(Serializer, "day-duration", Village.Day_Duration);
			IO(Serializer, "night-duration", Village.Night_Duration);
			if Serializer in DYAYaml.Reader or else Village.Victim_Existing then
				IO(Serializer, "victim-existing", Village.Victim_Existing);
				IO(Serializer, "victim-role", Village.Victim_Role);
			end if;
			IO(Serializer, "teaming", Village.Teaming, Default => Shuffling_Headless);
			IO(Serializer, "monster-side", Village.Monster_Side, Default => Fixed);
			IO(Serializer, "attack", Village.Attack, Default => Two);
			IO(Serializer, "servant-knowing", Village.Servant_Knowing, Default => None);
			IO(Serializer, "daytime-preview", Village.Daytime_Preview, Default => Role_And_Message);
			IO(Serializer, "doctor-infected", Village.Doctor_Infected, Default => Cure);
			IO(Serializer, "hunter-silver-bullet", Village.Hunter_Silver_Bullet, Default => Target_And_Self);
			IO(Serializer, "unfortunate", Village.Unfortunate, Default => None);
			if Serializer in DYAYaml.Reader or else Village.Appearance /= Role_Appearances'(others => Random) then
				IO(Serializer, "appearance", Appearance_Callback'Access);
			end if;
			IO(Serializer, "people", Village.People);
			if Serializer in DYAYaml.Reader or else Village.Escaped_People /= null then
				IO(Serializer, "escaped-people", Village.Escaped_People);
			end if;
			if not Info_Only then
				IO(Serializer, "messages", Village.Messages);
			end if;
		end Root_Callback;
	begin
		IO(Serializer, Root_Callback'Access);
	end IO;
	
end Tabula.Villages.Village_IO;
