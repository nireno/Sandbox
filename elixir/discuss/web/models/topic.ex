defmodule Discuss.Topic do
	use Discuss.Web, :model

	schema "Topic" do
		field :title, :string
	end

  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:title])
    |> validate_required([:title])
  end
end