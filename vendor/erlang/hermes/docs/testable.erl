f().
Port = testing:spawn_irb().
Command = "clouds[\"app\"].maximum_instances".
testing:irb_get(Port, Command).

testing:irb_get(Port, "clouds[\"app\"].minimum_instances").
testing:irb_get(Port, "clouds[\"app\"].name").
testing:irb_get(Port, "clouds[\"app\"].nodes").


testing:irb_get(Port, "clouds[\"app\"].cloud_provider.ec2.describe_images").
testing:irb_get(Port, "clouds[\"app\"].nodes").
testing:irb_get(Port, "clouds[\"app\"].run(\"ps\")").