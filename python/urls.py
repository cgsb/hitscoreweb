from django.conf.urls.defaults import *
from polls.models import Poll

# Uncomment the next two lines to enable the admin:
from django.contrib import admin
admin.autodiscover()

info_dict = {
    'queryset': Poll.objects.all(),
}

# urlpatterns = patterns('',
#     (r'^$', 'django.views.generic.list_detail.object_list', info_dict),
#     (r'^(?P<object_id>\d+)/$', 'django.views.generic.list_detail.object_detail', info_dict),
#     url(r'^(?P<object_id>\d+)/results/$', 'django.views.generic.list_detail.object_detail', dict(info_dict, template_name='polls/results.html'), 'poll_results'),
#     (r'^(?P<poll_id>\d+)/vote/$', 'polls.views.vote'),
# )

urlpatterns = patterns('',
    (r'^polls/$', 'polls.views.index'),
    (r'^polls/(?P<poll_id>\d+)/$', 'polls.views.detail'),
    (r'^polls/(?P<poll_id>\d+)/results/$', 'polls.views.results'),
    (r'^polls/(?P<poll_id>\d+)/vote/$', 'polls.views.vote'),

    # Example:
    # (r'^cgsb/', include('cgsb.foo.urls')),

    # Uncomment the admin/doc line below to enable admin documentation:
    (r'^admin/doc/', include('django.contrib.admindocs.urls')),

    # Uncomment the next line to enable the admin:
    (r'^admin/', include(admin.site.urls)),
)
