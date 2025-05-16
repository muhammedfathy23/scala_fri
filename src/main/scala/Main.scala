object CareerPathExplorer {

  // ====================== Domain Models ======================
  case class CareerField(name: String, description: String, requiredSkills: List[String])
  case class JobRole(
                      title: String,
                      field: String,
                      description: String,
                      skills: List[String],
                      responsibilities: List[String],
                      skillVideos: Map[String, String] // Maps skill name to video URL
                    )
  case class QuizQuestion(question: String, options: List[String], correctAnswer: String, hints: List[String])
  case class PersonalityQuestion(question: String, options: List[String], traitMapping: Map[String, String])
  case class Interaction(id: Int, userInput: String, botResponse: String)
  case class QuizInteraction(id: Int, question: String, userAnswer: String, evaluation: String)
  case class UserProfile(
                          username: String,
                          password: String,
                          email: String,
                          interactions: List[Interaction],
                          quizResults: List[QuizInteraction],
                          roleInquiries: Map[String, Int]
                        )
  case class UserCredentials(username: String, password: String)
  case class ChatState(
                        currentQuiz: Option[List[Either[QuizQuestion, PersonalityQuestion]]],
                        quizAnswers: List[String],
                        currentField: Option[String],
                        currentRole: Option[String],
                        awaitingQuizType: Boolean = false,
                        usedHints: List[String] = List(),
                        user: Option[UserProfile] = None
                      )

  // ====================== Core Chatbot ====================== fathy
  object CareerChatbot {
    // ====================== Levenshtein Algorithm ======================
    object Levenshtein {
      def levenshteinDistance(s1: String, s2: String): Int = {
        val len1 = s1.length
        val len2 = s2.length

        // Create a matrix to store distances
        val dp = Array.ofDim[Int](len1 + 1, len2 + 1)

        // Initialize first row and column
        for (i <- 0 to len1) dp(i)(0) = i
        for (j <- 0 to len2) dp(0)(j) = j

        // Fill the matrix
        for (i <- 1 to len1; j <- 1 to len2) {
          val cost = if (s1(i - 1) == s2(j - 1)) 0 else 1
          dp(i)(j) = math.min(
            math.min(dp(i - 1)(j) + 1,      // Deletion
              dp(i)(j - 1) + 1),     // Insertion
            dp(i - 1)(j - 1) + cost         // Substitution
          )
        }
        dp(len1)(len2)
      }
    }

    // Find the closest matching career field using Levenshtein distance
    def findClosestField(input: String, fields: List[CareerField], maxDistance: Int = 3): Option[String] = {
      fields.map { field =>
        (field.name, Levenshtein.levenshteinDistance(input.toLowerCase, field.name.toLowerCase))
      }.filter(_._2 <= maxDistance)
        .sortBy(_._2)
        .headOption
        .map(_._1)
    }

    // Find the closest matching job role using Levenshtein distance
    def findClosestRole(input: String, roles: List[JobRole], maxDistance: Int = 3): Option[JobRole] = {
      roles.map { role =>
        (role, Levenshtein.levenshteinDistance(input.toLowerCase, role.title.toLowerCase))
      }.filter(_._2 <= maxDistance)
        .sortBy(_._2)
        .headOption
        .map(_._1)
    }

    private def loadCareerFields(): List[CareerField] = {
      val source = scala.io.Source.fromResource("career_fields.csv")
      try {
        val lines = source.getLines().drop(1) // Skip header
        lines.map { line =>
          // Split by comma but respect quoted fields
          val parts = line.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)")
          if (parts.length != 3) {
            throw new IllegalArgumentException(s"Invalid CSV line format: $line")
          }
          val Array(name, description, skillsStr) = parts
          
          // Clean up the quoted strings
          val skills = skillsStr.stripPrefix("\"").stripSuffix("\"").split(",").toList
          
          CareerField(name.trim, description.trim, skills)
        }.toList
      } finally {
        source.close()
      }
    }

    private def loadJobRoles(): List[JobRole] = {
      val source = scala.io.Source.fromResource("job_roles.csv")
      try {
        val lines = source.getLines().drop(1) // Skip header
        lines.map { line =>
          // Split by comma but respect quoted fields
          val parts = line.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)")
          if (parts.length != 6) {
            throw new IllegalArgumentException(s"Invalid CSV line format: $line")
          }
          val Array(title, field, description, skillsStr, responsibilitiesStr, videosStr) = parts
          
          // Clean up the quoted strings
          val skills = skillsStr.stripPrefix("\"").stripSuffix("\"").split(",").toList
          val responsibilities = responsibilitiesStr.stripPrefix("\"").stripSuffix("\"").split(",").toList
          val skillVideos = videosStr.stripPrefix("\"").stripSuffix("\"")
            .split(",")
            .map { pair =>
              val Array(skill, url) = pair.split(":", 2)
              skill.trim -> url.trim
            }
            .toMap
          
          JobRole(title.trim, field.trim, description.trim, skills, responsibilities, skillVideos)
        }.toList
      } finally {
        source.close()
      }
    }

    val careerFields: List[CareerField] = loadCareerFields()
    val jobRoles: List[JobRole] = loadJobRoles()

    def greetUser(username: String): String = {
      s"Hello $username ! I'm your Career Path Explorer,are you looking for help ?"
    }

    def parseInput(input: String): List[String] = {
      input.toLowerCase
        .trim
        .split("\\s+")
        .filter(_.nonEmpty)
        .toList
    }

    def handleUserInput(input: String, state: ChatState, counter: Int): (String, ChatState, Int) = {
      val tokens = parseInput(input)  //----------------------------------> need to know more about parse function

      (tokens, state.awaitingQuizType, state.currentQuiz) match {
        case (words, false, None) if words.exists(w => w == "quiz" || w == "exam") =>
          val newState = state.copy(awaitingQuizType = true)
          return ("Would you prefer a knowledge quiz about careers or a personality assessment?", newState, counter)

        case (words, true, _) if words.exists(w => w == "knowledge" || w == "personality") =>
          val quizType = if (words.contains("knowledge")) "knowledge" else "personality"
          val selectedQuiz = QuizGenerator.selectQuizQuestions(quizType)
          val firstQuestion = selectedQuiz.head match {
            case Left(q) => formatQuestion(q)
            case Right(p) => formatQuestion(p)
          }
          val updatedState = state.copy(currentQuiz = Some(selectedQuiz), awaitingQuizType = false)
          return (s"Starting $quizType quiz:\n$firstQuestion", updatedState, counter)

        case (words, _, Some(_)) if words.exists(w => List("stop", "end", "quit", "exit", "cancel").contains(w)) =>
          val summary = AnalyticsDashboard.getInteractionSummary(state.user.get)
          val newState = state.copy(currentQuiz = None, awaitingQuizType = false, usedHints = List())
          return (s"Quiz ended. Here's your session summary:\n$summary", newState, 0)

        case _ => // fall through to rest of function
      }

      state.currentQuiz match {
        case Some(quiz) =>
          val (response, updatedState) = handleQuizResponse(input.toLowerCase.trim, quiz, state)
          (response, updatedState, counter)

        case None => tokens match {
          case words if (words.exists(w => w == "advice" || w == "career")) && counter == 0 =>
            provideCareerAdvice(input, state, counter)

          case words if words.exists(w => List("tech", "healthcare", "arts", "finance", "education", "engineering").contains(w)) && counter == 1 =>
            val field = findClosestField(input.toLowerCase, careerFields) match {
              case Some(f) => f
              case None =>
                // Fallback to original logic
                if (words.contains("tech")) "Tech"
                else if (words.contains("healthcare")) "Healthcare"
                else if (words.contains("arts")) "Arts"
                else if (words.contains("finance")) "Finance"
                else if (words.contains("education")) "Education"
                else "Engineering"
            }
            val updatedState = state.copy(currentField = Some(field))
            provideCareerAdvice(field, updatedState, counter)


//-----------------------------------> case for random role
          case words if words.exists(w => List("any thing","i do not know","suggest","anything","any role").contains(w)) && counter == 2 =>
                          val randomRole = jobRoles(scala.util.Random.nextInt(jobRoles.length))
                          val details = s"""I suggest you look into ${randomRole.title}:
                                          |Description: ${randomRole.description}
                                          |Key Skills: ${randomRole.skills.mkString(", ")}
                                          |____________________________________________
                                          |Would you like to know more about this role or explore another role?""".stripMargin
                          (details, state.copy(currentRole = Some(randomRole.title)), counter + 1)

          
            //-----------------------------------> case for specific role
          case s if counter == 2 && state.currentField.isDefined =>
            // Check if user is asking about a specific role
            val inputLower = input.toLowerCase.trim
            val fieldRoles = jobRoles.filter(_.field.equalsIgnoreCase(state.currentField.get))
            val role = findClosestRole(inputLower, fieldRoles) match {
              case Some(r) => Some(r)
              case None =>
                // Fallback to original logic
                jobRoles.find(r => r.title.toLowerCase.contains(inputLower) || inputLower.contains(r.title.toLowerCase))
            }
            role match {
              case Some(r) =>
                val details = s"""Role: ${r.title}
                                 |Description: ${r.description}
                                 |Key Skills: ${r.skills.mkString(", ")}
                                 |____________________________________________
                                 |Would you like to know about more role or know more informations about this role or explore another field?""".stripMargin
                (details, state.copy(currentRole = Some(r.title)), counter + 1)
              case None =>
                val fieldRoles = jobRoles.filter(_.field.equalsIgnoreCase(state.currentField.get))
                val roleList = fieldRoles.map(_.title).mkString(", ")
                ("I didn't recognize that role. Please try again with one of these roles: " + roleList, state, counter)
            }



//-----------------------------------> case for another role or field or more information about role
          case s if (s.contains("another") || s.contains("more")) && counter >= 3 =>
            if (s.contains("roles") || s.contains("role")) {
              state.currentField match {
                case Some(field) =>
                  val roles = jobRoles.filter(_.field.equalsIgnoreCase(field))
                  val roleList = roles.map(_.title).mkString(", ")
                  (s"Here are the roles in $field again: $roleList. Which one would you like to know more about?", state, counter - 1)
                case None =>
                  ("Please first select a field to see its roles.", state, 1)
              }
            }
            else if (s.contains("field") || s.contains("fields")) {
              println(counter)
              ("Which field are you interested in? Options: " + careerFields.map(_.name).mkString(", "), state, 1)
            }
            else if ((s.contains("information") || s.contains("details")||s.contains("info")||s.contains("informations")) && state.currentRole.isDefined) {
              jobRoles.find(_.title.equalsIgnoreCase(state.currentRole.get)) match {
                case Some(role) =>
                  val extendedInfo =
                    s"""Detailed Information about ${role.title}:
                       |Field: ${role.field}
                       |Description: ${role.description}
                       |------------------------------------
                       |Key Skills:
                       |${role.skills.map(skill => s"- $skill (Learn more: ${role.skillVideos.getOrElse(skill, "Video not available")})").mkString("\n")}
                       |______________________________
                       |Typical Responsibilities:
                       |${role.responsibilities.map(r => s"- $r").mkString("\n")}
                       |________________________________
                       |Would you like to explore another role or field or take quiz?""".stripMargin
                  (extendedInfo, state, counter + 1)
                case None =>
                  ("Sorry, I couldn't retrieve details for this role.", state, counter)
              }
            }
            else {
              state.currentField match {
                case Some(field) =>
                  (s"Would you like to see the roles in $field again or need more information about this role or explore another field?", state, counter)
                case None =>
                  ("Would you like to explore more roles or another field?", state, counter)
              }
            }

          case s if (s.exists(w=>List("hi","welcome","hello","3aml eh","hey","hi there","hello there","hey there","good morning","good afternoon","good evening","marhaba","salam","salamo alaikom").contains(w)))=>
            ("Hello! I'm your friendly Career Path Explorer chatbot. I'm here to help you discover your career path! \nWould you like career advice or to take a quiz?",state,0)

          case s if (s.contains("good")||s.contains("fine")||s.contains("nice")||s.contains("kawyes")||s.contains("great")||s.contains("excellent")||s.contains("perfect")||s.contains("awesome")||s.contains("amazing")||s.contains("wonderful"))=>
            ("That's fantastic to hear! \nWould you like to explore career options or take a quiz to discover your career path?",state,0)

          case s if (s.contains("bad")||s.contains("not good")||s.contains("terrible")||s.contains("awful")||s.contains("mish kwayes")||s.contains("mish tamam"))=>
            ("I'm sorry to hear that. Remember, every day is a new opportunity! \nWould you like to explore some exciting career paths? It might help lift your spirits!",state,0)

          case s if s.exists(w => List("thank","thanks","thank you","shokran","merci","grazie","gracias").contains(w)) =>
            ("You're very welcome! \nLet me know if you need any more career guidance or have questions about specific roles.", state, 0)

          case s if s.exists(w => List("bye","goodbye","see you","farewell","ma3 el salama","bye bye","exit","quit").contains(w)) =>
            ("Goodbye! Don't forget to check your analytics before leaving. Hope to see you again soon! 👋", state, 0)

          case s if (s.exists(w => List("help","need help","can you help","help me","saa3dni","help please","yes","yeah","yep","sure","okay","ok","alright","tab3an","aywa","na3am").contains(w))) =>
            ("""I'm here to help! Here's what I can do:
               |. Provide career advice and explore different fields
               |. Take a career quiz to discover your interests
               |. Show detailed information about specific roles
               |. Share learning resources for different skills
               |. Tell you a joke to lighten the mood
               |
               |Just let me know what you'd like to do! """.stripMargin, state, 0)

          case s if (s.exists(w => List("joke","nokta","moz7a","jokes","funny","make me laugh","tell me a joke").contains(w))) =>
            val jokes = List(
              "Why don't programmers like nature? It has too many bugs!",
              "How many programmers does it take to change a light bulb? None, that's a hardware problem!",
              "Why did the developer go broke? Because he used up all his cache!",
              "ما هو اسم الشجرة التي لا تثمر؟ الشجرة العاقلة!",
              "لماذا لا يستخدم المبرمجون الشامبو؟ لأنهم يفضلون Conditioner!",
              "ماذا قال السيرفر للسيرفر الآخر؟ حياتي كلها request و response!",
              "Why did the computer go to the doctor? Because it had a virus!",
              "What do you call a computer that sings? A Dell!",
              "Why was the math book sad? Because it had too many problems!",
              "What did the ocean say to the beach? Nothing, it just waved!"
            )
            val randomJoke = jokes(scala.util.Random.nextInt(jokes.length))
            (s"$randomJoke\n\nWould you like to hear another joke, get career advice, or take a quiz? 😄", state, 0)

          case s if (s.exists(w => List("what","how","why","when","where","who","which","explain","tell me about","describe","define").contains(w))) =>
            ("""I can help you with:
               |. Career information and advice
               |. Job role descriptions and requirements
               |. Skills needed for different careers
               |. Learning resources for specific skills
               |_________________________________________
               |What would you like to know more about? """.stripMargin, state, 0)

          case s if (s.exists(w => List("sorry","apologize","excuse me","pardon","3afwan","sorry about that").contains(w))) =>
            ("No need to apologize! I'm here to help. What would you like to explore? ", state, 0)

          case s if (s.exists(w => List("yes","yeah","yep","sure","okay","ok","alright","tab3an","aywa","na3am").contains(w))) =>
            ("Great! Would you like to explore career options or take a quiz to discover your career path? ", state, 0)

          case s if (s.exists(w => List("no","nope","nah","never","la2","ma3lesh").contains(w))) =>
            ("That's okay! Is there something else you'd like to explore? I'm here to help with any career-related questions! ", state, 0)

        
          case s if (s.exists(w => List("boring","not interesting","mish mohem","not helpful").contains(w))) =>
            ("I'm sorry to hear that. Let me try to make it more interesting! Would you like to:\n. Hear a career-related joke\n. Take a fun personality quiz\n. Explore some unique career paths\nWhat would you prefer? 🌟", state, 0)

          case _ =>
            println(state.currentField)
            println(counter)
            println("------------------")
            val newstate = state.copy(currentField = None)
            println(newstate.currentField)
            ("I'm not sure I understand. Would you like to:\n . Get career advice\n . Take a quiz\n . tell you a joke \n . Get help\n Please let me know what you'd like to do! ", newstate, 0)
        }
      }
    }


    private def provideCareerAdvice(input: String, state: ChatState, counter: Int): (String, ChatState, Int) = {
      if (state.currentField.isEmpty) {
        val fields = careerFields.map(_.name).mkString(", ")
        val message = "Great! Which field are you interested in? Options: " + fields
        val newState = state.copy(currentField = None)
        (message, newState, counter + 1)
      } else {
        input match {
          case s if careerFields.exists(_.name.equalsIgnoreCase(s)) =>
            val field = careerFields.find(_.name.equalsIgnoreCase(s)).get
            val roles = jobRoles.filter(_.field.equalsIgnoreCase(s))
            val roleList = roles.map(_.title).mkString(", ")
            (s"""${field.name} is a great choice! 
                |Description: ${field.description}
                |Required Skills: ${field.requiredSkills.mkString(", ")}
                |_____________________________________________
                |Here are some roles in this field: $roleList.
                |Which role would you like to know more about?""".stripMargin,
              state.copy(currentField = Some(field.name)), counter + 1)
          case _ =>
            ("Please specify a career field from the options provided.", state, counter)
        }
      }
    }


    private def startQuiz(input: String, state: ChatState): (String, ChatState) = {
      ("Would you prefer a knowledge quiz about careers or a personality assessment?",
        state.copy(currentQuiz = Some(QuizGenerator.selectQuizQuestions("knowledge"))))
    }

    private def handleQuizResponse(answer: String, quiz: List[Either[QuizQuestion, PersonalityQuestion]], state: ChatState): (String, ChatState) = {
      quiz match {
        case Left(q) :: tail =>
          val help = List(
            "don't know", "dont know", "no idea", "not sure", "idk", "i don't know",
            "help", "hint", "need help", "give me a hint", "can you help",
            "stuck", "confused", "not clear", "unclear",
            "what should i", "how do i", "what do you mean",
            "maybe", "perhaps", "probably", "might be", "could be",
            "this is hard", "too difficult", "struggling with",
            "show hint", "next hint", "another hint", "more hints"
          )

          if (help.exists(pattern => answer.toLowerCase.contains(pattern))) {
            val unusedHints = q.hints.filterNot(hint => state.usedHints.contains(hint))

            unusedHints match {
              case hint :: remainingHints =>
                val newState = state.copy(usedHints = hint :: state.usedHints)
                (s"That's okay! Here's a hint: $hint\nTry again with one of the options:\n${formatQuestion(q)}",
                  newState)

              case Nil =>
                if (state.usedHints.nonEmpty) {
                  (s"I'm out of hints! You must choose one of these options:\n${formatQuestion(q)}",
                    state)
                } else {
                  (s"You need to choose one of these options:\n${formatQuestion(q)}",
                    state)
                }
            }
          } else {
            val evaluation = QuizGenerator.evaluateAnswer(Left(q), answer)
            val updatedUser = AnalyticsDashboard.logQuizInteraction(q.question, answer, evaluation, state.user.get)
            val updatedState = state.copy(user = Some(updatedUser))

            if (q.correctAnswer.equalsIgnoreCase(answer.trim) || evaluation.startsWith("Incorrect")) {
              if (tail.isEmpty) {
                (s"$evaluation\nQuiz complete! Type 'exit' to see your analytics.",
                  updatedState.copy(currentQuiz = None, usedHints = List()))
              } else {
                val nextQuestion = formatQuestion(tail.head.left.get)
                (s"$evaluation\nNext question:\n$nextQuestion",
                  updatedState.copy(currentQuiz = Some(tail), usedHints = List()))
              }
            } else {
              (s"$evaluation\nPlease choose one of these options:\n${formatQuestion(q)}",
                updatedState)
            }
          }

        case Right(p) :: tail =>
          val evaluation = QuizGenerator.evaluateAnswer(Right(p), answer)
          val matchedOption = p.options.find(opt => answer.toLowerCase.contains(opt.toLowerCase))

          matchedOption match {
            case Some(opt) =>
              val updatedUser = AnalyticsDashboard.logQuizInteraction(p.question, answer, evaluation, state.user.get)
              val updatedState = state.copy(user = Some(updatedUser))
              
              if (tail.isEmpty) {
                (s"$evaluation\nBased on your answers, you might enjoy careers in: [result]. Type 'exit' to see analytics.",
                  updatedState.copy(currentQuiz = None))
              } else {
                val nextQuestion = formatQuestion(tail.head.right.get)
                (s"$evaluation\nNext question:\n$nextQuestion",
                  updatedState.copy(currentQuiz = Some(tail)))
              }
            case None =>
              val currentQuestion = formatQuestion(p)
              (s"$evaluation\nPlease answer again:\n$currentQuestion", state.copy(currentQuiz = Some(quiz)))
          }

        case Nil =>
          ("Quiz complete! Type 'exit' to see your analytics.", state.copy(currentQuiz = None))
      }
    }

    private def formatQuestion(question: QuizQuestion): String = {
      // Using list operations and string interpolation
      val numberedOptions = question.options
        .zipWithIndex
        .map { case (option, index) => s"${index + 1}) $option" }
        .mkString("\n")

      s"""${question.question}
         |Choose one option:
         |$numberedOptions""".stripMargin
    }

    private def formatQuestion(question: PersonalityQuestion): String = {
      val numberedOptions = question.options
        .zipWithIndex  // (programming,0)
        .map { case (option, index) => s"${index + 1}) $option" }
        .mkString("\n")

      s"""${question.question}
         |Choose one option:
         |$numberedOptions""".stripMargin
    }
  }


  // ====================== Quiz Generator ====================== hannah
  object QuizGenerator {
    import scala.util.Random

    private def loadKnowledgeQuiz(): List[QuizQuestion] = {
      val source = scala.io.Source.fromResource("knowledge_quiz.csv")
      try {
        val lines = source.getLines().drop(1) // Skip header
        lines.map { line =>
          val parts = line.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)")
          if (parts.length != 4) {
            throw new IllegalArgumentException(s"Invalid CSV line format: $line")
          }
          val Array(question, optionsStr, correctAnswer, hintsStr) = parts
          
          // Clean up the quoted strings
          val options = optionsStr.stripPrefix("\"").stripSuffix("\"").split(",").toList
          val hints = hintsStr.stripPrefix("\"").stripSuffix("\"").split(",").toList
          
          QuizQuestion(
            question.stripPrefix("\"").stripSuffix("\""),
            options,
            correctAnswer.stripPrefix("\"").stripSuffix("\""),
            hints
          )
        }.toList
      } finally {
        source.close()
      }
    }

    private def loadPersonalityQuiz(): List[PersonalityQuestion] = {
      val source = scala.io.Source.fromResource("personality_quiz.csv")
      try {
        val lines = source.getLines().drop(1) // Skip header
        lines.map { line =>
          val parts = line.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)")
          if (parts.length != 3) {
            throw new IllegalArgumentException(s"Invalid CSV line format: $line")
          }
          val question = parts(0).stripPrefix("\"").stripSuffix("\"")
          val optionsStr = parts(1).stripPrefix("\"").stripSuffix("\"")
          val traitMappingStr = parts(2).stripPrefix("\"").stripSuffix("\"")
          
          val options = optionsStr.split(",").toList
          val traitMapping = traitMappingStr.split(",")
            .map { pair =>
              val keyValue = pair.split(":", 2)
              if (keyValue.length != 2) {
                throw new IllegalArgumentException(s"Invalid trait mapping format: $pair")
              }
              (keyValue(0).trim, keyValue(1).trim)
            }
            .toMap
          
          PersonalityQuestion(question, options, traitMapping)
        }.toList
      } finally {
        source.close()
      }
    }

    val knowledgeQuiz: List[QuizQuestion] = loadKnowledgeQuiz()
    val personalityQuiz: List[PersonalityQuestion] = loadPersonalityQuiz()

    def selectQuizQuestions(quizType: String): List[Either[QuizQuestion, PersonalityQuestion]] = {
      val questions = quizType match {
        case "knowledge" => Random.shuffle(knowledgeQuiz).map(Left(_))
        case "personality" => Random.shuffle(personalityQuiz).map(Right(_))
        case _ => List()
      }
      questions
    }

    def evaluateAnswer(question: Either[QuizQuestion, PersonalityQuestion], answer: String): String = {
      def getRandomHint(hints: List[String]): Option[String] =
        if (hints.isEmpty) None
        else Some(Random.shuffle(hints).head)

      question match {
        case Left(q) =>
          val help= List(

            "don't know", "dont know", "no idea", "not sure", "idk", "i don't know","mesh 3aref","mesh 3arfa", "help", "hint", "need help", "give me a hint", "can you help", "stuck", "confused", "not clear", "unclear", "what should i", "how do i", "what do you mean", "maybe", "perhaps", "probably", "might be", "could be",
            "this is hard", "too difficult", "struggling with","show hint", "next hint", "another hint", "more hints"
          )

          if (help.exists(pattern => answer.toLowerCase.contains(pattern))) {
            getRandomHint(q.hints) match {
              case Some(hint) => s"That's okay! Here's a hint: $hint\nTry again with one of the options!"
              case None => "That's okay! Try your best guess from the options!"
            }
          } else if (q.correctAnswer.equalsIgnoreCase(answer.trim)) {
            "Correct!"
          } else {
            s"Incorrect. The correct answer is ${q.correctAnswer}"
          }

        case Right(p) =>
          val matchedOption = p.options.find(opt => answer.toLowerCase.contains(opt.toLowerCase))
          matchedOption match {
            case Some(opt) =>
              val traitResult = p.traitMapping.getOrElse(opt, "unknown")
              s"You seem suited for $traitResult careers"
            case None =>
              "Sorry, I couldn't understand your answer. Please try again using one of the options."
          }
      }
    }

  }

  // ====================== Analytics Dashboard ======================
  object AnalyticsDashboard {
    private var interactions: List[Interaction] = List()
    private var quizResults: List[QuizInteraction] = List()
    private var interactionCount: Int = 0

    def logInteraction(userInput: String, botResponse: String, user: UserProfile): UserProfile = {
      interactionCount += 1
      interactions = Interaction(interactionCount, userInput, botResponse) :: interactions
      user.copy(interactions = Interaction(interactionCount, userInput, botResponse) :: user.interactions)
    }

    def logQuizInteraction(question: String, userAnswer: String, evaluation: String, user: UserProfile): UserProfile = {
      interactionCount += 1
      quizResults = QuizInteraction(interactionCount, question, userAnswer, evaluation) :: quizResults
      user.copy(quizResults = QuizInteraction(interactionCount, question, userAnswer, evaluation) :: user.quizResults)
    }

    def getInteractionSummary(user: UserProfile): String = {
      val total = user.interactions.size + user.quizResults.size
      val careerTopics = user.interactions.count(i =>
        i.userInput.toLowerCase.contains("career") ||
          i.userInput.toLowerCase.contains("advice"))
      val quizAttempts = user.quizResults.size
      val mostPopularField = getMostPopularField(user)
      val personalityTrends = getPersonalityTrends(user)

      s"""|=== Career Path Explorer Analytics for ${user.username} ===
          |Total interactions: $total
          |Career advice requests: $careerTopics
          |Quiz attempts: $quizAttempts
          |Most discussed field: ${mostPopularField.getOrElse("None")}
          |Personality trends: ${personalityTrends.getOrElse("No data")}
          |""".stripMargin
    }

    private def getMostPopularField(user: UserProfile): Option[String] = {
      val fieldCounts = CareerChatbot.careerFields.map { field =>
        val count = user.interactions.count(_.userInput.toLowerCase.contains(field.name.toLowerCase))
        (field.name, count)
      }
      fieldCounts.maxByOption(_._2).map(_._1)
    }

    private def getPersonalityTrends(user: UserProfile): Option[String] = {
      if (user.quizResults.isEmpty) None
      else {
        val traits = user.quizResults.flatMap { q =>
          QuizGenerator.personalityQuiz.find(_.question == q.question).map { pq =>
            pq.traitMapping.getOrElse(q.userAnswer, "unknown")
          }
        }
        val grouped = traits.groupBy(identity).view.mapValues(_.size).toMap
        Some(grouped.map { case (k, v) => s"$k ($v)" }.mkString(", "))
      }
    }
  }

  // ====================== User Storage ======================
  object UserStorage {
    import java.io._

    def saveUser(profile: UserProfile): Unit = {
      val dir = new File("users")
      if (!dir.exists()) dir.mkdir()
      val writer = new PrintWriter(new FileWriter(s"users/${profile.username}.txt"))
      try {
        writer.println(s"username:${profile.username}")
        writer.println(s"password:${profile.password}")
        writer.println(s"email:${profile.email}")
        writer.println("interactions:")
        profile.interactions.foreach { i =>
          writer.println(s"  id:${i.id},input:${i.userInput},response:${i.botResponse}")
        }
        writer.println("quizResults:")
        profile.quizResults.foreach { q =>
          writer.println(s"  id:${q.id},question:${q.question},answer:${q.userAnswer},evaluation:${q.evaluation}")
        }
        writer.println("roleInquiries:")
        profile.roleInquiries.foreach { case (role, count) =>
          writer.println(s"  $role:$count")
        }
      } finally {
        writer.close()
      }
    }

    def loadUser(username: String): Option[UserProfile] = {
      try {
        val lines = scala.io.Source.fromFile(s"users/$username.txt").getLines().toList
        var password = ""
        var email = ""
        var interactions: List[Interaction] = List()
        var quizResults: List[QuizInteraction] = List()
        var roleInquiries: Map[String, Int] = Map()
        var currentSection = ""

        lines.foreach { line =>
          if (line.startsWith("username:")) {
            // Skip username line (already known)
          } else if (line.startsWith("password:")) {
            password = line.stripPrefix("password:")
          } else if (line.startsWith("email:")) {
            email = line.stripPrefix("email:")
          } else if (line.trim == "interactions:") {
            currentSection = "interactions"
          } else if (line.trim == "quizResults:") {
            currentSection = "quizResults"
          } else if (line.trim == "roleInquiries:") {
            currentSection = "roleInquiries"
          } else if (line.trim.startsWith("id:") && currentSection == "interactions") {
            val parts = line.trim.split(",", 3)
            val id = parts(0).stripPrefix("id:").toInt
            val input = parts(1).stripPrefix("input:")
            val response = parts(2).stripPrefix("response:")
            interactions = Interaction(id, input, response) :: interactions
          } else if (line.trim.startsWith("id:") && currentSection == "quizResults") {
            val parts = line.trim.split(",", 4)
            val id = parts(0).stripPrefix("id:").toInt
            val question = parts(1).stripPrefix("question:")
            val answer = parts(2).stripPrefix("answer:")
            val evaluation = parts(3).stripPrefix("evaluation:")
            quizResults = QuizInteraction(id, question, answer, evaluation) :: quizResults
          } else if (line.trim.contains(":") && currentSection == "roleInquiries") {
            val Array(role, count) = line.trim.split(":", 2)
            roleInquiries = roleInquiries.updated(role, count.toInt)
          }
        }
        Some(UserProfile(username, password, email, interactions.reverse, quizResults.reverse, roleInquiries))
      } catch {
        case _: Exception => None
      }
    }

    def userExists(username: String): Boolean = {
      new File(s"users/$username.txt").exists()
    }

    def saveUsersList(users: List[UserCredentials]): Unit = {
      val dir = new File("users")
      if (!dir.exists()) dir.mkdir()
      val writer = new PrintWriter(new FileWriter("users/users_list.txt"))
      try {
        users.foreach { u =>
          writer.println(s"${u.username}:${u.password}")
        }
      } finally {
        writer.close()
      }
    }

    def loadUsersList(): List[UserCredentials] = {
      try {
        val lines = scala.io.Source.fromFile("users/users_list.txt").getLines().toList
        lines.map { line =>
          val Array(username, password) = line.split(":", 2)
          UserCredentials(username, password)
        }
      } catch {
        case _: Exception => List()
      }
    }

    def addUserToList(credentials: UserCredentials): Unit = {
      val currentUsers = loadUsersList()
      val updatedUsers = credentials :: currentUsers.filter(_.username != credentials.username)
      saveUsersList(updatedUsers)
    }
  }

  // ====================== Authentication ======================
  object Authentication {
    def signup(): UserProfile = {
      println("Enter username:")
      val username = scala.io.StdIn.readLine().trim
      if (username.isEmpty || UserStorage.userExists(username)) {
        println("Username invalid or already taken. Try again.")
        return signup()
      }
      println("Enter password:")
      val password = scala.io.StdIn.readLine().trim
      if (password.isEmpty) {
        println("Password cannot be empty. Try again.")
        return signup()
      }
      println("Enter email:")
      val email = scala.io.StdIn.readLine().trim
      if (!email.contains("@") || email.isEmpty) {
        println("Invalid email. Try again.")
        return signup()
      }
      val profile = UserProfile(username, password, email, List(), List(), Map())
      UserStorage.saveUser(profile)
      UserStorage.addUserToList(UserCredentials(username, password))
      println(s"Account created for $username!")
      profile
    }

    def login(): Option[UserProfile] = {
      println("Enter username:")
      val username = scala.io.StdIn.readLine().trim
      println("Enter password:")
      val password = scala.io.StdIn.readLine().trim
      UserStorage.loadUser(username) match {
        case Some(profile) if profile.password == password =>
          println(s"Welcome back, $username!")
          Some(profile)
        case _ =>
          println("Invalid username or password. Try again or type 'signup' to create an account.")
          None
      }
    }

    def authenticate(): UserProfile = {
      println("Type 'login' to log in or 'signup' to create an account:")
      val choice = scala.io.StdIn.readLine().trim.toLowerCase
      choice match {
        case "signup" => signup()
        case "login" =>
          login() match {
            case Some(profile) => profile
            case None => authenticate()
          }
        case _ =>
          println("Invalid choice. Please type 'login' or 'signup'.")
          authenticate()
      }
    }
  }

  // ====================== Main Program ======================
  def main(args: Array[String]): Unit = {
    val userProfile = Authentication.authenticate()
    var state: ChatState = ChatState(None, List(), None, None, user = Some(userProfile))

    println("\nBot: " + CareerChatbot.greetUser(userProfile.username))
    val updatedUser = AnalyticsDashboard.logInteraction("System", "Chat started", userProfile)
    state = state.copy(user = Some(updatedUser))

    var running = true
    var counter = 0
    while (running) {
      println("\nUser: ")
      val input = scala.io.StdIn.readLine()
      val updatedUserInput = AnalyticsDashboard.logInteraction(input, "", state.user.get)
      state = state.copy(user = Some(updatedUserInput))

      if (input.equalsIgnoreCase("exit")) {
        println("\nBot: Here's your session summary:")
        println(AnalyticsDashboard.getInteractionSummary(state.user.get))
        running = false
      } else {
        val (response, newState, newCounter) = CareerChatbot.handleUserInput(input, state, counter)
        println("\nBot: " + response)
        val updatedUserResponse = AnalyticsDashboard.logInteraction(input, response, newState.user.get)
        state = newState.copy(user = Some(updatedUserResponse))
        counter = newCounter
      }
    }
  }
}