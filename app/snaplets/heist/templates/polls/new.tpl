<apply template="base">
    <bind tag="pagetitle">New poll</bind>
    <form method="POST">
        <fieldset>
            <label for="title">Title</label><br />
            <input type="text" name="title" id="title" required="required" /><br />
            <label for="description">Description</label><br />
            <textarea id="description" name="description"></textarea><br />
            <label for="start">Start</label><br />
            <input type="datetime" name="start" id="start" required="required" placeholder="Format: 2014-04-24 10:00:00" /><br />
            <label for="end">End</label><br />
            <input type="datetime" name="end" id="end" required="required" placeholder="Format: 2014-04-24 12:00:00" /><br />
            <input type="submit" value="Submit" />
        </fieldset>
    </form>
</apply>