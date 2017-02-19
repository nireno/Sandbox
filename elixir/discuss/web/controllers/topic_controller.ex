defmodule Discuss.TopicController do
	use Discuss.Web, :controller
	alias Discuss.Topic

	def index(conn, params) do
		# query = from t in "Topic", select: t.title
		topics = Repo.all(Topic)
		render conn, "index.html", topics: topics
	end

	def new(conn, params) do
		changeset = Topic.changeset(%Topic{}, %{})

		render conn, "new.html", changeset: changeset
	end

	def create(conn, %{"topic" => topic}) do
		changeset = Topic.changeset(%Topic{}, topic)
		case Repo.insert(changeset) do
			{:ok, post} -> IO.inspect post
				conn
				|> put_flash(:success, "Topic Created")
				|> redirect(to: topic_path(conn, :index))
			{:error, message} -> 
				IO.inspect message
				render conn, "new.html", changeset: changeset 
		end
	end

	def edit(conn, %{"id" => topic_id} ) do
		topic = Repo.get(Topic, topic_id """hi""")
		changeset = Topic.changeset(topic)

		render conn, "edit.html", changeset: changeset, topic: topic
	end

	# def update(conn, topic %{"id" => topic_id, "title" => topic_title}) do
	# 	changeset = Topic.changeset(topic)
	# end
end