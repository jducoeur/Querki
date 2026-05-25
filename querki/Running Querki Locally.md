In general, we strongly recommend using the real version of Querki (in the cloud, at querki.net) for real work. Convenience aside, running a solid Querki install requires a lot of horsepower: at minimum, about eight nodes if you want everything to be robust and reliable, with sufficient redundancy.

But Querki is intentionally open-source, and local installs for non-commercial use are legit. Here are concise instructions for setting up an install. But keep in mind, it's still pretty involved. This is approximately how I do my own development.

The following assumes that you are doing your development on a Mac; that's where Scala development tends to happen nowadays. It's possible to do it all on Windows (Querki was originally developed using Windows machines), but things tend to be clunkier, and I haven't tried recently.

### Initial Setup

These are one-time tasks, to get your development environment built.

* **Install Homebrew**. In practice, it's almost impossible to do any interesting development on a Mac without Homebrew.
* **Install Python and set up a venv**. See the CCM documentation (linked below) for some useful tips on how to do this. Suffice it to say, the built-in Mac Python setup doesn't work well in practice. Assuming you've created a venv named `venvawslocal`, whenever you need to do any of the Python work you should prep the current shell as:
```
source venvawslocal/bin/activate
```
* **Install sbt**. Querki is entirely written in Scala, and uses [sbt](https://www.scala-sbt.org/) to build. It doesn't matter which version of sbt you install; it automatically self-installs the correct version for Querki when it builds.
* **Install an IDE**. Developing Scala using a raw editor kind of sucks. In general, you want to either use IntelliJ IDEA (Community Edition works fine), or the Scala Metals environment with Visual Studio Code or some other LSP-compatible editor.
* **Clone this repo**. All the source code for Querki itself is here; the other open-source components will be downloaded during the build.
* **Install MySQL 5.7**. For the moment, we aren't yet compatible with 8.0. (We'll get there, but there's work to do, and it's not high priority.) MySQL 5.7 is pretty fully deprecated at this point; [here are some useful tips for installing it](https://gist.github.com/robhrt7/392614486ce4421063b9dece4dfe6c21?permalink_comment_id=3402084).
* **Set up the Querki account in MySQL**. As the root MySQL user, create an account to use. You *may* want this to be named to match your host login; that makes logging into the MySQL shell slightly easier, but that's a minor goodie. Assuming that this account is named `querki`, you need to
```
create user 'querki'@'localhost' identified by '<password>';
```
* **Create Querki Databases**. Create two DBs, named `querkisystem` and `querkiuser`.
* **Grant permissions to the Querki account**:
```
GRANT SELECT,INSERT,UPDATE,DELETE,CREATE,DROP,INDEX,REFERENCES on querkisystem.* TO 'querki'@'localhost';
GRANT SELECT,INSERT,UPDATE,DELETE,CREATE,DROP,ALTER on querkiuser.* TO 'querki'@'localhost';
```
* **OLD: Install ccm**. ~~Querki relies on Cassandra for the bulk of the userland data, but setting up a full Cassandra install is a huge headache for a local copy. That is where [ccm](https://github.com/riptano/ccm) comes in: it creates and runs a small Cassandra pseudo-cluster on localhost. It isn't robust enough for commercial use, but it's great for development, and suffices for modest use.~~
* **OLD: Create a ccm cluster**. ~~Tell ccm to use Cassandra 3.0.6, which is what Querki is currently designed for. You can call it whatever you like; the name isn't relevant for Querki.~~
* **Install Docker**. Querki uses Docker for deployment. It's up to you whether to use Docker Desktop (some folks don't like the license) or an alternative.
* **Install AWS LocalStack**. At least for the time being, Querki relies on a few native AWS services, so you'll want to have LocalStack running for those. The Community Edition will suffice -- we're not using the fancier features that need the Pro Edition.
* **Create application.conf**. In your Querki repo, find the file `querki/scalajvm/conf/application.conf.template`. Copy that to `application.conf`, and fill it all in. There is a *lot* there, and there will be a bunch of things to consider.
* **Create the secrets**. Then find the file `secrets.conf.template`. Copy that somewhere that is **not** checked in (either in an external folder or covered by `.gitignore`). The name doesn't matter, but we'll say `localsecrets.conf` as an example. Generate the secret values expected there.
* **Build Querki**. In the `Querki/querki` directory, run `sbt` (which will download a lot of stuff); when that stops, say `compile` (which will download a lot more).
* **Figure out passwords**. Open the sbt Console, and import `querki.security.EncryptionUtil`. That gives you access to the `calcHash()` function. Feed that your desired admin account password, and 20000 iterations. (Or however many iterations you said in `application.conf`.) That will spit out the hashed admin password.
* **Fill in all.sql**. Find the file `querki/scalajvm/conf/evolutions/default/all.sql`. Copy it somewhere, and edit it to suit yourself, filling in all the lines marked **TODO**. This includes the system and admin passwords, which you computed above.
* **Run all.sql**. Now, feed that file to MySQL, for the querkisystem database. This will create the system tables. (The querkiuser database will build itself as you go.)

### Each Time You Start the Machine

Local development requires that LocalStack and Cassandra be running. You can do these manually, or put them into a boot-time script.

* **Run LocalStack**. In a terminal window running a Python venv, boot up LocalStack with:
```
LOCALSTACK_HOSTNAME_EXTERNAL=host.docker.internal localstack start
```
* **Put the secrets into LocalStack**. The secrets go into the LocalStack Secrets Manager. (Note that this is transient; you'll need to redo it after each reboot.) Assuming your secrets file is named `localsecrets.conf`, in another terminal window set up the pyenv, and then in the directory holding the secrets file say:
```
awslocal secretsmanager create-secret --name querki/app/secrets --secret-string "$(< localsecrets.conf)"
```
* **OLD: Start ccm**. ~~Say `ccm start` to run Cassandra. If you get an error, say:~~
```
sudo ifconfig lo0 alias 127.0.0.2 up
```
```
sudo ifconfig lo0 alias 127.0.0.3 up
```
* **Start Cassandra in Docker.** Open a terminal to `querki/local/cassandra`, and say:
```
docker compose up -d
```
See the `README.md` file there for more options.

### Building and Running Querki

* **Create the Querki Docker image**: in order to create a current Docker image, go into sbt, and say:
```
docker:publishLocal
```
* **Run Querki**. Here goes! You've created and published the local Docker image; run that from the command line as:
```
docker run --rm -e QUERKI_ENV=local -e AWS_ACCESS_KEY_ID=XXXXXXXXXXXX1 -e AWS_SECRET_ACCESS_KEY=XXXXXXXXXXXX2 -p 9000:9000 querkiserver:3.x.x.x
```  
Update the version number to whatever the current version is. The AWS key values are irrelevant, but need to exist in order to boot.
* **Log in and use Querki**. When that settles down, navigate to `http://localhost:9000`, and theoretically it should show you Querki's login page. Try logging in as the admin.