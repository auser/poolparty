module S3String
  def bucket_objects
    AWS::S3::Bucket.objects(self)
  end
  def bucket_object(key)
    AWS::S3::S3Object.value key, self if bucket_object_exists?(key)
  end
  def bucket_object_exists?(key)
    AWS::S3::S3Object.exists? key, self
  end
  def store_bucket_value(key, data)
    AWS::S3::S3Object.store key, data, self unless bucket_object_exists?(key)
  end
  def delete_bucket_value(key)
    AWS::S3::S3Object.delete(key, self) if bucket_object_exists?(key)
  end
  def bucket_exists?
    begin
      AWS::S3::Bucket.find(self)
      return true
    rescue
      return false
    end        
  end
  def delete_bucket
    AWS::S3::Bucket.delete(self, :force => true) if bucket_exists?
  end
end

class String
  include S3String
end