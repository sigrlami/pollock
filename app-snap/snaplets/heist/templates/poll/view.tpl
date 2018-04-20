<apply template="base">
    <bind tag="pagetitle"><polltitle/></bind>
    <h3><polltitle /></h3>
    <p>
	<polldescription/>
    </p>
    <h4> #<pollid /> | <pollstart/> - <pollend/></h4>
    <h4>by <pollowner/></h4>
    <form action="/poll/delete/${pollid}" method="POST">
        <input type="submit" value="Delete poll" />
    </form>
</apply>